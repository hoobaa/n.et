(in-package 'n.et)

;;;;;; TODO remove finally.
;; (setq n-debug-p t)

#| c symbol load |#
(eval-always
  (ff-load-c-fn
   "accept" "bind" "close" "fcntl"
   ;;"htonl" 
   ;;"htons" 
   "kevent" "kqueue" "listen" "pipe"
   "read" "readv" 
   "setsockopt" "socket"
   "write" "writev")

  (ff-load-c-constant
   "EAGAIN" 
   "EVFILT_READ" "EVFILT_WRITE" 
   "EV_ADD" "EV_ENABLE" "EV_DELETE" "EV_EOF" "EV_ONESHOT" 
   "F_GETFL" "F_SETFL" 
   "O_NONBLOCK" 
   "AF_INET" "PF_INET" "AF_LOCAL"
   "SOL_SOCKET" 
   "SO_REUSEADDR"
   "SOCK_STREAM")

 ;; #+freebsd
 ;; (progn
 ;;   (ff-load-c-var
 ;;    "errno"
 ;;    )
 ;;   (defun errno ()
 ;;     #$errno)
 ;;   )
 ;; #+darwin
 ;; (defun errno ()
 ;;   ;;(ccl:ff-call (foreign-symbol-entry "__error") :integer)
 ;;   (ccl::%get-long (#___error))
 ;;   )
 )

#| util |#
(defmacro assert-c (ret msg)
  `(assert (not (eql -1 ,ret))
           () "CERROR. msg:~a errno:~a" ,msg (ff-errno-by-txt)))
(defmacro assert-c2 (expr msg)
  `(assert ,expr
           () "CERROR. msg:~a errno:~a" ,msg (ff-errno-by-txt)))

(def** bytes-cat (&rest bytes-s)
  (let* ((len (reduce '+ (mapcar (lambda (x) (length x)) bytes-s)))
       (ret (make-array len :element-type '(unsigned-byte 8))))
    (loop 
       with i = 0
       for v in bytes-s
       do 
       (replace ret v :start1 i)
       (incf i (length v)))
    (values ret len)))

(def** oct-search (seq tgt &optional (start 0) (end (length seq)))
  (declare (type bytes seq tgt) (type (byte 32) start end))
  "NOW USE VER."
  (let ((tgt-len (length tgt)))
    (loop for i from start below (1+ (- end tgt-len))
       do (when (loop for j below tgt-len
                   while (eql (aref seq (+ j i)) 
                              (aref tgt j))
                   finally (if (eql tgt-len j)
                               (return t)
                               (return nil)))
            (return i))
       finally (progn
                 (return nil)))))

(def** oct-search-in-ring-buffer (seq tgt &optional (start 0) (len 0))
  "ring buffer. TODO:performance
   return relative position from start"
  (let ((seq-len (length seq))
      (tgt-len (length tgt)))
    (loop 
       for i from start below (1+ (- (+ start len) tgt-len))
       do (when (loop for j below tgt-len
                   while (eql (aref seq (mod (+ j i) seq-len)) ;;;; maybe mod is slow.
                              (aref tgt j))
                   finally (if (eql tgt-len j)
                               (return t)
                               (return nil)))
            (return (- i start)))
       finally (return nil))))

;;;;;;;;;;;;;;;;;;;;
;;;; simple buffer ;;;;
(defstruct (sbuf
             (:print-function
              (lambda (p s k)
                (format s "#S(SBUF len:~a con-start:~a con-len:~a)" (sbuf-len p) (sbuf-con-start p) (sbuf-con-len p)))))
  (ivec nil  :type bytes)
  (len  1024 :type (byte 32))

  (con-start 0 :type (byte 32)) ;; content start
  (con-len   0 :type (byte 32)))

(def** sbuf-gen (ivec &optional (con-start 0) (con-len (- (length ivec) con-start)))
  (make-sbuf :ivec ivec :len (length ivec) :con-start con-start :con-len con-len))

(def** sbuf-con-proceed (sbuf n) ;; TODO:ring
  (incf (sbuf-con-start sbuf) n)
  (decf (sbuf-con-len sbuf) n))

;;;;;;;;;;;;;;;;;;;;
;;;; ring buffer ;;;;;; 
(defstruct (ringbuf
             (:print-function
              (lambda (p s k)
                (format s "#S(RINGBUF len:~a con-start:~a con-len:~a)" (ringbuf-len p) (ringbuf-con-start p) (ringbuf-con-len p)))))
  (ivec nil  :type bytes)
  (len  1024 :type (byte 32))
  (con-start 0 :type (byte 32)) ;; content start
  (con-len   0 :type (byte 32)))

(def** ringbuf-gen (len)
  (make-ringbuf :ivec (make-array len :element-type 'byte)
   :len len))

(def** ringbuf2ivec (ringbuf &optional (con-len nil))
  (let* ((ivec (ringbuf-ivec ringbuf))
       (len (ringbuf-len ringbuf))
       (con-len (or con-len (ringbuf-con-len ringbuf)))
       (con-start (ringbuf-con-start ringbuf))
       (con-end (+ con-start con-len)))
    (ef (<= con-end len)
        (values (ringbuf-ivec ringbuf) con-start con-len)

      (let ((res-ivec (make-array con-len :element-type 'byte))
          (con-len1 (- len con-start)))
        (replace res-ivec ivec :start1 0        :end1 con-len1 :start2 con-start :end2 len)
        (replace res-ivec ivec :start1 con-len1 :end1 con-len  :start2 0         :end2 (- len con-len1))
        (values res-ivec 0 con-len)))))

(def** ringbuf-proceed (ringbuf n &key (con-len-p t))
  (declare (type (byte 32) n))
  "readp meen content append mode. socket read and append it."
  ;; TODO term check.
  (ef con-len-p
      (incf (ringbuf-con-len ringbuf) n)

    (decf (ringbuf-con-len   ringbuf) n)
    (let ((len (ringbuf-len ringbuf))
        (old-con-start (ringbuf-con-start ringbuf)))
      (setf (ringbuf-con-start ringbuf)
            (mod (+ old-con-start n) len)))))

(def** ringbuf-prepare (ringbuf len)
  (let* ((cur-len (ringbuf-len ringbuf))
       (len-to-add
        (- len
           (- cur-len (ringbuf-con-len ringbuf)))))

    (when (< 0 len-to-add) ;; must create
      (let ((new-len-min (+ cur-len len-to-add))
          (new-len cur-len)
          new-ivec)
        (assert (<= new-len-min #.(* 512 1024 1024)))
        (loop 
           do (setf new-len (* 2 new-len))
           until (<= new-len-min new-len))

        ;; ivec
        (let* ((cur-con-len    (ringbuf-con-len ringbuf))
             (con-start1 (ringbuf-con-start ringbuf))
             (con-end1   (+ con-start1 cur-con-len))
             con-len1

             (con-start2 0)
             (con-end2   0)
             (con-len2   0)

             (cur-ivec (ringbuf-ivec ringbuf)))

          (when (< cur-len con-end1)
            (setf con-end2 (- con-end1 cur-len))
            (setf con-end1 cur-len)
            (setf con-len2 (- con-end2 con-start2)))

          (setf con-len1 (- con-end1 con-start1))

          (setf new-ivec (make-array new-len :element-type 'byte))
          (replace new-ivec cur-ivec :start1 0 :end1 con-len1 :start2 con-start1 :end2 con-end1)
          (when (< 0 con-len2)
            (replace new-ivec cur-ivec :start1 con-len1 :end1 cur-con-len  :start2 con-start2 :end2 con-end2))
          (setf (ringbuf-ivec ringbuf)      new-ivec
                (ringbuf-len ringbuf)       new-len
                (ringbuf-con-start ringbuf) 0
                (ringbuf-con-len ringbuf)   cur-con-len)

          ringbuf)))))

(def** ringbuf-iovec-info (ringbuf &key (readp t) ;; when readp, append content after src-con-end(mod (+ src-con-start src-con-len) src-len)
                                   &aux 
                                   (src-len (ringbuf-len ringbuf))
                                   src-con-start 
                                   src-con-len 
                                   (iovecs-len 1) f1 t1 (f2 0) (t2 0))
  "get iovec's info. RET: ivec, iovecs-len(1 or 2), #(#(from-idx len) #(from-idx len))"
  (ef readp
      (setf src-con-start (mod (+ (ringbuf-con-start ringbuf) (ringbuf-con-len ringbuf)) src-len)
            src-con-len (- src-len (ringbuf-con-len ringbuf)))
    (setf src-con-start (ringbuf-con-start ringbuf)
          src-con-len (ringbuf-con-len ringbuf)))

  (setf f1 src-con-start)
  (setf t1 (+ f1 src-con-len))
  (let ((len src-len))
    (when (< len t1)
      (setf t1 len
            t2 (- src-con-len (- t1 f1))
            iovecs-len 2)))
  (values (ringbuf-ivec ringbuf) iovecs-len `#(#(,f1 ,(- t1 f1)) #(,f2 ,(- t2 f2)))))

(def** ringbuf-prepare-if-need (ringbuf)
  "if there is no capa to append, extend buffer."
  (when (<= (ringbuf-len ringbuf) (ringbuf-con-len ringbuf))
    (ringbuf-prepare ringbuf (1+ (ringbuf-len ringbuf)))))

;;;;;;;;;;;;;;;;;;;;;; condition
(define-condition ioc-reg-error (simple-error)
  ()
  (:report "ioc-reg-error"))
(define-condition ioc-write-finish-error (simple-error)
  ((:errno :initarg :errno :initform 0))
  (:report "ioc-write-finish-error"))
(define-condition ioc-end-of-file (end-of-file)
  ()
  (:report "ioc-end-of-file"))

(define-condition sock-creation-error (simple-error)
  ((:errno :initarg :errno :initform 0))
  (:report "sock-creation-error"))

