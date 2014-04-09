(in-package 'n.et)

#| kq |#
(eval-always
  (defvar kq-fd 0) ;; if 0, loop stop.

  (defvar kq-cons nil)
  (defvar kq-cons-len 1024)

  (defvar kq-evts-len-max 512)
  (defvar kq-evts-out nil)
  (defvar kq-evts-in nil)
  (defvar kq-evts-len-in 0)

  (defvar kq-timeout-nsec 600000000)

  (defvar kq-loop-proc nil) ;; thread...TODO:change to unix process base.

  (defvar kq-suspended-vps (make-tlist)
    "resumed vps. use nil as a delimiter.") ;; nq is thread-safe. performance may be bad.

  (defvar kq-notify-rfd 0)
  (defvar kq-notify-wfd 0)
  )

(def kq-init (&key (cons-len 16384) (forcep nil))
  (when (< 0 kq-fd)
    (ef forcep
        (kq-fin)

      (error "kq-init fail because kq-fd>0 and forcep is nil")
      ))

  (setq kq-fd (#_kqueue))

  (setq kq-cons-len cons-len)
  (setq kq-cons (make-array cons-len :initial-element nil))

  (setq kq-evts-in (ff-anew :kevent kq-evts-len-max))
  (setq kq-evts-out (ff-anew :kevent kq-evts-len-max))
  (setq kq-evts-len-in 0)
  (ccl:rlet ((fildes (:array :i32 2)))
   (let ((ret (#_pipe fildes))) ;; 0-read 1-write
     (assert-c ret "notify-pipe gen error")
     (setq kq-notify-rfd (ff-pref fildes :ui32)
           kq-notify-wfd (ff-pref (ff-inc fildes 4) :ui32))))

  (fcntl kq-notify-rfd #$O_NONBLOCK) ;; #$O_NONBLOCK ;; ccl bug?

  (ccl:rlet ((rbuf (:array :ui8 64)))
   (let ((ret (#_read kq-notify-rfd rbuf 64))) ;; for kq
     (logd ">>>notify first read res. ret:~s errno:~s" ret (ff-errno-by-txt))

     (let ((ret (kq-reg-raw-one kq-notify-rfd #$EVFILT_READ #$EV_ADD)))
       (assert ret () "kq-reg error"))))

  (logd "kq inited")
  )

(def* kq-timeout-set (nsec &optional (forcep nil))
  "to set kevent's timeout. if forcep is nill, use min value as the timeout."
  (ef forcep
      (setq kq-timeout-nsec nsec)
    (setq kq-timeout-nsec (min kq-timeout-nsec nsec))))

(let ((ptr (ff-new :timespec :tv_sec 1 :tv_nsec 0)))
  (def* kq-timeout-ptr ()
    (multiple-value-bind (sec nsec) (floor kq-timeout-nsec 1000000)
      (setf (ff-pref ptr :timespec.tv_sec) sec
            (ff-pref ptr :timespec.tv_nsec) nsec)
      ptr)))

(defmacro kq-con-get (fd)
  `(svref kq-cons ,fd))
(defmacro kq-con-set (fd v)
  `(setf (svref kq-cons ,fd) ,v))

(def** kq-con-close (con &aux fd)
  ;; delete registerd timout
  (setq fd (con-fd con))

  (when (eq con (kq-con-get fd)) 
    (kq-con-set fd nil) ;; remove con from kq-cons

    ;; I'll clean up kq entry.
    (kq-reg con #$EVFILT_READ #$EV_DELETE 0 0 nil)
    (kq-reg con #$EVFILT_WRITE #$EV_DELETE 0 0 nil)
    (kq-reg con #$EVFILT_TIMER #$EV_DELETE 0 0 nil))

  (con-close con)
  )

(def** kq-con-gen (fd &key kevt rbuf )
  (logd ">>kq-con-gen 0")

  (awhen (kq-con-get fd) ;; dead con already exits. reset.
    (loge "@kq-con-gened con. con conflict. I'll close old con and if continuation exists call it with condition old-con:~s " it)
    (when (con-closed-p it)
      (con-close-softly it)
      (awhen (con-kevt-pop it) ;; continuatin
        (funcall (kevt-k it) "ERROR new connection generating. your con is closed"))))

  (logd ">>kq-con-gen 1. fd:~a" fd)

  ;; con set new con
  (kq-con-set 
   fd
   (con-gen fd :kevt kevt :rbuf rbuf))
  )

;;;;
(defmacro kevt-set (evt-ptr fd filter flags)
  `(setf (ff-pref ,evt-ptr :kevent.ident) ,fd
         (ff-pref ,evt-ptr :kevent.filter) ,filter
         (ff-pref ,evt-ptr :kevent.flags) ,flags))
(defstruct kevt ;; old-name kq-evt-req
  (fd     0 :type (byte 32))
  (filter 0 :type (sbyte 32))
  (flags  0 :type (byte 32))
  (fflags 0 :type (byte 32))
  (data   0 :type (byte 64))

  (k nil :type (or null function)) ;; keizoku
  )

(eval-always ;; to avoid runtiime error condition. load foreign type information when load.
  (ff-load-c-type
   '(:array (struct :kevent)))
  )

(def* kq-reg (con filter flags fflags data k
                   &aux 
                   kevt fd
                   evts-ptr-in)
  "RETURN: nil if cannot reg.
ARGS: k is continuation-closure. if k is nil then this func won't set continuation to con."

  (logd ">>>>KQ-REG con:~s filter:~s flags:~s data:~s" con filter flags data)

  (setq fd (con-fd con))

  (when k ;; set-continuation.

    (when (con-closed-p con)
      (logw "@kq-reg. con is already closed. con:~s" con)
      (return-from kq-reg nil))

    (when (con-kevt con)
      (loge "@kq-reg. con-kevt is already exists. con:~s. do nothing. process conflict?" con)
      (return-from kq-reg nil))
    (setf kevt (make-kevt :fd fd :filter filter :flags flags :fflags fflags :data data :k k))
    (setf (con-kevt con) kevt) ;; soft kevent reg.
    )

  (when (>= kq-evts-len-in (1- kq-evts-len-max)) ;; buffer is 2
    (let ((res (#_kevent kq-fd 
                       kq-evts-in kq-evts-len-max
                       ff-nil 0
                       ff-nil)))
      (when (= -1 res)
        (loge "@kq-reg because of full of buffer. res=-1. errno:~a" (ff-errno-by-txt))
        (return-from kq-reg nil))
      (setq kq-evts-len-in 0)))

  (setq evts-ptr-in (ff-paref kq-evts-in (struct :kevent) kq-evts-len-in)) ;; inline function and C-struct is cause of trouble.
  
  (incf kq-evts-len-in)
  (setf (ff-pref evts-ptr-in :kevent.ident) fd
        (ff-pref evts-ptr-in :kevent.filter) filter
        (ff-pref evts-ptr-in :kevent.flags) flags
        (ff-pref evts-ptr-in :kevent.fflags) fflags
        (ff-pref evts-ptr-in :kevent.data) data
        (ff-pref evts-ptr-in :kevent.udata) ff-nil
        )
  t
  )

(def** kq-reg-raw-one (fd filter flags)
  (ccl:rletz ((evts (:array (:struct kevent) 1)))
    (kevt-set evts fd filter flags)
   (let ((res (#_kevent kq-fd evts 1 ff-nil 0 ff-nil)))
      (ef (= -1 res)
          (progn
            (logw "@kq-reg-raw-one #_kevent return -1. fd:~a filter:~a flags:~a errno:~a" fd filter flags (ff-errno-by-txt))
            nil)
        t))))

(let ((_last-uuid 1)
      (_vp-conds (ht-gen :test 'equal))) "for debug..."
      (def** call-vp (vp)
        "call vp. vp is closure which realize hand-made thread. wannabe of erlang like LWP."
        (logd "call-vp -------")
        (handler-case
            (funcall vp)
          (error (c)
           (incf _last-uuid)
           ;;(setq _last-uuid (mkstr (uuid:make-v4-uuid)))
           (ht _vp-conds _last-uuid (list c (ccl::backtrace-as-list)))
           (let (c-txt)
            (ignore-errors
              (setq c-txt (format nil "~a" c)))
            (loge "ERROR@call-vp.UUID:~s. C:~s(~a).fd:~s" _last-uuid c c-txt fd)))
          (ccl::invalid-memory-access (c)
           ;;(setq _last-uuid (mkstr (uuid:make-v4-uuid)))
           (incf _last-uuid)
           (ht _vp-conds _last-uuid (list c (ccl::backtrace-as-list)))
           (let (c-txt)
            (ignore-errors
              (setq c-txt (format nil "~a" c)))
            (loge "ERROR@call-vp.UUID:~s. C:~s(~a).fd:~s" _last-uuid c c-txt fd)))
          ))

      (def call-vp-condition ( &key (uuid _last-uuid) )
        "for debug.."
        (ht _vp-conds uuid))
      )

(def* KQ-LOOP (&aux evts-len) ;; TODO: I wonder if I can implement this loop with series or generator-gatherer like-stuffs.
  ;; wait
  (setq evts-len (#_kevent kq-fd
                           kq-evts-in  kq-evts-len-in
                           kq-evts-out kq-evts-len-max
                           (kq-timeout-ptr)
                           ))
  (cond
    ((eql -1 evts-len)
     (logd "KQ-LOOP: #_kevent returns error. I'll continue to next loop. errno:~s" (ff-errno-by-txt)) ;; TODO: except EINTR, handle
     (KQ-LOOP))

    (t

     (logd "KQ-LOOP START")
     ;; I reset previous loop information.
     (kq-timeout-set 600000000 t)
     (setq kq-evts-len-in 0)

     ;; ;; I call suspended vps.
     ;; (do ((vp #1=(ccl:with-lock-grabbed ((nq-lock kq-suspended-vps))
     ;;     ((null vp)) ;; untile it meets the delimiter (nil).
     ;;   (logd "call-vp")
     ;;   (call-vp vp))

     (tagbody
      retry
        (when (tlist-empty-p kq-suspended-vps)
          (go end))
        (let ((vp (tlist-rem-left kq-suspended-vps)))
          (unless vp
            (logd ">>>> vp is nil. break loop")
            (go end))
          (logd "call-vp >>>>")
          (call-vp vp))
      end
        )
     
     ;; I handle received events.
     (dotimes (evt-i evts-len)
       (let* ((evt-ptr (ff-paref kq-evts-out (:struct :kevent) evt-i))

            (fd (ff-pref evt-ptr :kevent.ident))
            (filter (ff-pref evt-ptr :kevent.filter))
            (flags (ff-pref evt-ptr :kevent.flags))
            (fflags (ff-pref evt-ptr :kevent.fflags))
            (data (ff-pref evt-ptr :kevent.data))

            (con (kq-con-get fd)))

         ;; (logd "KQ-LOOP: I'll handle received event. FD:~s FILTER:~s FLAGS:~s FFLAGS:~s DATA:~s" fd filter flags fflags data)

         (cond
           ((= kq-notify-rfd fd) ;; notifyed.
            (ccl:rlet ((rbuf (:array :ui8 64)))
              (let ((ret (#_read kq-notify-rfd rbuf 64)))
                (logd "KQ-LOOP: notified and read ~a bytes." ret))))

           ((null con)
            (logw "KQ-LOOP: I can't find con from kq-cons. I'll do nothing. recv-evt fd:~a filter:~a flags:~a fflags:~a" fd filter flags fflags))

           ((and 
             (member filter (list #$EVFILT_TIMER #$EVFILT_WRITE #$EVFILT_READ))
             (/= 0 (logand flags #$EV_ERROR))
             (= data #$ENOENT))
            ;; nop
            )
           ((con-kevt con) ;; found continuation
            (let ((kevt (con-kevt con)))
              (cond
                ((= #$EVFILT_TIMER filter)
                 (logd ">>>>TIMEOUT. fd:~s" (con-fd con))
                 (kq-con-close con)
                 ;; (ef (and (/= 0 (logand flags #$EV_ERROR)) 
                 ;;          (= data #$ENOENT)) ;; always occer?
                 ;;     (progn                  ;; error
                 ;;       ;; it ignores if TIMER ENOENT ERROR.
                 ;;       )
                 ;;   (logd ">>>>TIMEOUT. fd:~s" (con-fd con))
                 ;;   (kq-con-close con)))
                 )

                ;; HERE is MAIN VP CALL.
                ((or (= filter (kevt-filter kevt)) ;; receive the event which con is waiting for.
                     (and (= #$EVFILT_READ filter) (/= 0 (logand #$EV_EOF flags)))) ;; if EOF, call continuation because there may be buffered data left.
                 (setf (con-kevt con) nil) ;; pop con's k(continuation)
                 (call-vp (kevt-k kevt)))    ;; and call poped k.

                (t ;; I receive unexpected event, I'llunregister it.
                 (logd "KQ-LOOP ERROR reged-filter(~a) and rcv-filter(~a) are different. I'll unreg event." filter (kevt-filter kevt))
                 (kq-reg con filter #$EV_DELETE 0 0 nil)
                 ))))

           ((and (/= 0 (logand #$EV_EOF flags)) ;; receive EOF & at con is no continuation then close con.
                 (member filter (list #$EVFILT_READ #$EVFILT_WRITE)))
            (loge "KQ-LOOP cannnot find continuation and EOF is receive. I'll kq-con-close")
            (kq-con-close con))

           (t ;; I receive unexcepted event, I'll unregister it.
            (logd "KQ-LOOP cannot find continuation. fd:~a filter:~a flags:~a fflags:~a data:~a). I'll unreg event." 
                  fd filter flags fflags data)
            16384
            #$EVFILT_WRITE
            (kq-reg con filter #$EV_DELETE 0 0 nil)
            ))))

     ;;(ccl:with-lock-grabbed ((nq-lock kq-suspended-vps))
     (when (tlist-right kq-suspended-vps)
       ;; if any vps is added in the below processes, I must enq the delimiter.
       (tlist-add-right kq-suspended-vps nil))
     ;;)

     (KQ-LOOP))))

(def kq-loop-notify ()
  (ccl:rlet ((wbuf (:array :ui8 1)))
   (let ((ret (#_write kq-notify-wfd wbuf 1)))
      t)))

#| io |#
(-def io-read-some (con timeout)
  "timeout is milisec."
 (let ((rbuf (con-rbuf con)))
    (ringbuf-prepare-if-need rbuf))

 (let ((read-len (con-read-some con)))
    (cond
      ((eql 0 read-len)
       (logw "@con-read-some. return is 0. I'll kq-con-close. vals condition. fd:~a" (con-fd con))
       (kq-con-close con)
       (-signal 'ioc-end-of-file))

      ((= -1 read-len)                ;; non-block. wait-io.
       (unless (kq-reg con #$EVFILT_READ #$EV_ADD 0 0
                       (l*()
                         ;;(logd "ke-zoku of io-read-some. fd:~s" (con-fd con))
                         (io-read-some con timeout)))
         (-signal 'ioc-reg-error))
       (when timeout
         (kq-reg con #$EVFILT_TIMER (logior #$EV_ADD #$EV_ONESHOT) 0 timeout nil)) ;; reg timeout
       )

      (t
       (when timeout
         (kq-reg con #$EVFILT_TIMER #$EV_DELETE 0 0 nil)) ;; timeout reset
       (-vals read-len))
      )))

(-def io-read-until (con tgt timeout)
 (let* ((rbuf (con-rbuf con))
       (rlen (oct-search-in-ring-buffer
              (ringbuf-ivec rbuf)
              tgt
              (ringbuf-con-start rbuf) 
              (ringbuf-con-len rbuf))))
    (ef rlen ;; return relative position from start if found.
        (let ((result-ringbuf (copy-ringbuf rbuf)))
          (ringbuf-proceed rbuf rlen :con-len-p nil)
          (-vals result-ringbuf))

      (-bind (dum) (io-read-some con timeout)
        (io-read-until con tgt timeout)))))

(-def io-read-len (con len timeout)
 (let ((rbuf (con-rbuf con)))
    (ef (<= len (ringbuf-con-len rbuf)) ;; return relative position from start if found.
        (let ((result-ringbuf (copy-ringbuf rbuf)))
          (ringbuf-proceed rbuf len :con-len-p nil)
          (-vals result-ringbuf))

      (-bind (dum) (io-read-some con timeout)
        (io-read-len con len timeout)))))

(-def io-write-finish (con)
  (multiple-value-bind (res errno) (con-write-finish con)
    (logd "io-write-finish res is :~a" res)
    (case res
      (0
       (-vals t))
      (-2
       (kq-con-close con)
       (-signal 'ioc-write-finish-error :errno errno))
      (-3
       (kq-con-close con)
       (-signal 'ioc-end-of-file)
       )
      (t ;; -1 or rest anything
       (unless (kq-reg con #$EVFILT_WRITE (logior #$EV_ADD #$EV_ONESHOT) 0 0 
                       (l*()
                         (io-write-finish con)))
         (-signal 'ioc-reg-error))))))

(-def io-accept (con)
  "return raw fd"
 (let* ((lfd (con-fd con))
       (nfd (#_accept lfd ff-nil ff-nil)))
    (ef (= -1 nfd) ;; TODO:handle error code except non-block return.
        (progn
          (unless (kq-reg con #$EVFILT_READ #$EV_ADD 0 0
                          (l*() 
                            (io-accept con)))
            (-signal 'ioc-reg-error)))

      (fcntl nfd #$O_NONBLOCK)
      (-vals nfd))))

#| virtual proces |#
(def** vp-suspend (vp) ;; vp is pure clozure.

  (logd "vp suspended:~a" vp)
  ;;(ccl:with-lock-grabbed ((nq-lock kq-suspended-vps))
  (tlist-add-right kq-suspended-vps vp)
  ;;)

  (kq-timeout-set 0))

(def* vp-enq (vp)
  "You can use this when you wanna scheudle vp from another thread than kq-loop."
  (vp-suspend vp) 
  (kq-loop-notify)  ;; notify to interupt kq-loop and handle vps.
  (sleep 0.3)
  (kq-loop-notify)  ;; notify to make kq-loop handle vp which I'd qed.
  )

;;;; to generate vp which accept and generate new vp with new fd.
(def vp-gen-acceptor (lfd nfd-handler &aux lcon) ;; generate acceptor
  ;; TODO: can set socket parameter of lfd by function's argument.
  (setq lcon (kq-con-gen lfd))
  (alambda* ()
    ;;(logd ">>>> vp-of-acceptor. lcon:~a" lcon)
    (logd "START LISTENING . I'll start accept in gened acceptor. lfd:~s" (con-fd lcon))
    (-cs/hdl
     (progn
       (-bind (nfd) (io-accept lcon) ;; TODO handle condition
        (logd "NEW CONNECTION . I got new-fd. I call nfd-handler for new-fd. lfd:~s new-fd:~s" (con-fd lcon) nfd)
        (funcall nfd-handler nfd) ;; TODO condition.
        (self))
       )

     (error (c) (logd "accept-error. c:~s(~a)" c c))
     )))

