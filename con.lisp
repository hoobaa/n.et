(in-package 'n.et)

;;;;;;;; TODO: rbuf ring buffer.

#| con |#
(defstruct (con
             (:print-function
              (lambda (p s k)
                (format s "#(CON fd:~a closed-p:~a kevt-null:~a)" 
                     (con-fd p) (con-closed-p p) (null (con-kevt p))))))
  (fd 0     :type (unsigned-byte 32))
  (rbuf nil :type (or null ringbuf)) ;; ringbuf
  (wbuf (make-tlist) :type tlist)

  (kevt) ;; kevt for keizoku.

  (closed-p nil :type boolean) ;; for connection buffer.
  )

(def** con-gen (fd &key kevt rbuf)
  (make-con :fd fd :kevt kevt
            :rbuf rbuf))

(def** con-kevt-pop (con)
  (awhen (con-kevt con)
    (setf (con-kevt con) nil)
    it)
  nil)

;; io ;;;;;;;;;;;;;
(def** con-close-softly (con)
  "does not close fd."
  (setf (con-closed-p con) t))
(def** con-close (con)
  (logd ">>con-close ~s" con)
  (unless (con-closed-p con)
    (#_close (con-fd con))
    (setf (con-closed-p con) t)))

(def** con-write (con &rest objs)
  (dolist (obj objs)
    (tlist-add-right (con-wbuf con) obj)))

(def** con-write-finish (con &aux sbuf)
  "return: 0:wrote-all, n>0:rest-anything, -1:again, -2:error, -3:eof"
  (when (con-closed-p con)
    (return-from con-write-finish -3))

  ;;(setf sbuf (tlist-left (con-wbuf con)))
  (when (tlist-empty-p (con-wbuf con))
    (logd "con-write-finish. con wbuf is empty")
    (return-from con-write-finish 0)) ;; empty

  (setf sbuf (tlist-rem-left (con-wbuf con)))
  (logd "con-write-finish. write write write")

  ;; write to socket
  (multiple-value-bind (rest-len errno) (sock-write! (con-fd con) sbuf)

    ;; TODO: clean up return code.
    ;; TODO: easily, flush buffer.

    (cond
      ((= 0 rest-len)

       (ef (tlist-empty-p (con-wbuf con))
           (return-from con-write-finish 0)
         (return-from con-write-finish 1)))
       
       ;; (multiple-value-bind (dum qidx) (tlist-rem-left (con-wbuf con))
       ;;   (ef (= 1 qidx)
       ;;       0 ;; wrote all
       ;;     1))) ;; rest-anything
      (t
       (when (member errno '(-2 -3))
         (con-close con))
       (values rest-len errno)))))

(def** con-read-some (con &aux rbuf)
  "RETURN: -1:non-block return, 0:must to close con, n>0: read-length."

  (when (con-closed-p con) ;; TODO already read data return.
    (return-from con-read-some 0))

  (setf rbuf (con-rbuf con))

  (multiple-value-bind (read-len errno) (sock-read-with-ringbuf (con-fd con) rbuf)
    (cond 
      ((eql -1 read-len) ;; kqueue error handle.
       (cond
         ;; ((eql errno #$EAGAIN) ;; again
         ((eql errno 35)
          -1)
         (t
          (logw "@con-read-some. I'll close fd and return 0. fd:~a. errno:~a(~a)" (con-fd con) errno (ff-errno2txt errno))
          (con-close con)
          0)))
      ((eql 0 read-len)
       (logd "@con-read-some EOF. I'll close fd and return 0. fd:~a. close. " (con-fd con))
       (con-close con)
       0)
      (t
       read-len ;; read-len
       ))))

;; io to ;;;;;;;;;;;

(defmacro con-rbuf-proceed (con n &key (con-len-p t))
  `(ringbuf-proceed (con-rbuf ,con) ,n :con-len-p ,con-len-p))

(def con-debug-txt (con &aux rbuf)
  "for debug."
  (setf rbuf (con-rbuf con))
  (multiple-value-bind (ivec start len) (ringbuf2ivec rbuf)
    (o2t
     ivec
     :start start
     :end (+ start len))))



