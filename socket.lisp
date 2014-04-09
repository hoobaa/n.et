(in-package 'n.et)

#| address |#
(defmacro ip-enc (v)
  "endian is host"
  `(o2val ,v :ui32))
(defmacro ip-dec (i)
  "endian is host"
  `(val2o ,i :ui32))

(defmacro sockaddr (ip-v port)
  "struct definition @ include/netinet/in.h"
  `(ff-new :sockaddr_in
           :sin_family #$AF_INET
           :sin_port (ccl::htons ,port) 
           :sin_addr (ff-new :in_addr :s_addr (ccl::htonl (ip-enc ,ip-v)))))

#| ns |#
(declaim (inline name2addrs name2addr))
(def name2addrs (name &aux hostent)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ff-w/txts ((ptr name))
    (setf hostent (#_gethostbyname ptr)))
  (when (eql ff-nil hostent)
    (return-from name2addrs (values nil (list (ff-errno) (ff-errno-by-txt)))))
  (let ((hlen (ff-pref hostent :hostent.h_length)))
    (cond 
      ((eql hlen 4)
       (loop
          with res
          with aptr = (ff-pref hostent :hostent.h_addr_list)
          for len below 10
          for ptr = aptr then (ff-inc ptr 8)
          for addr = (ff-pref ptr :ptr)
          while (not (eql ff-nil addr))
          do (push (ccl::htonl (ff-pref addr :ui32)) res) 
          finally (return (values res len))))
      (t
       (error "not support address")))))

(def name2addr (name)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (addrs len) (name2addrs name)
    (unless addrs
      (return-from name2addr nil))
    (nth (random len) addrs)))

#| sock |#
(def** fcntl (fd val)
  (#_fcntl fd #$F_SETFL :int (logior (#_fcntl fd #$F_GETFL) val)))

(def** sock-gen ( &key (AF #$AF_INET) &aux fd)
  (setf fd (#_socket AF #$SOCK_STREAM 0))
  fd)

(def* sock-con (ip-v port &aux fd) ;; DO NOT INLINE
  (setq fd (sock-gen))
  (let ((ret (#_connect fd (sockaddr ip-v port) 16)))
    (when (eql -1 ret)
      (return-from sock-con (values -1 (ff-errno))))
    (fcntl fd #$O_NONBLOCK)
    fd))

(def* sock-listen (ip-v port &aux fd)
  (setf fd (sock-gen))
  (ccl:rlet ((opt-reuseaddr :int 1))
    opt-reuseaddr ;; TODO:remove.
    (#_setsockopt fd #$SOL_SOCKET #$SO_REUSEADDR opt-reuseaddr 4))
  (let ((ret (#_bind fd (sockaddr ip-v port) 16)))
    (assert-c ret "bind-error"))
  (let ((ret (#_listen fd 16)))
    (assert-c ret "listen-error"))
  (fcntl fd #$O_NONBLOCK)
  (logd ">>sock-listen started. fd:~s" fd)
  fd)

;;;; unix domain 
(def sock-listen-un (path &aux fd addr)
  (ff-w/txts ((path-ptr path))
    (#_unlink path-ptr))
  (setq fd (sock-gen :AF #$AF_LOCAL))
  (setq addr (ff-new :sockaddr_un))
  (#_bzero addr (ff-size :sockaddr_un))
  (setf (ff-pref addr :sockaddr_un.sun_family) #$AF_LOCAL)
  (let ((sun-path (ff-pref addr :sockaddr_un.sun_path)))
    (dotimes (i (length path))
      (setf (ff-pref (ff-inc sun-path i) :ui8) (char-code (char path i)))))
  (#_bind fd addr  (ff-size :sockaddr_un))

  (logd ">>> path:~s fd:~s" path fd)
  (ff-w/txts ((path-ptr path))
    (#_chmod path-ptr #o777))

  (#_listen fd 16)
  (fcntl fd #$O_NONBLOCK)
  fd)

(def** sock-read-with-ringbuf (fd ringbuf) ;;  ivec l-iovecs &aux (l-iovecs-len (length l-iovecs)))
  (multiple-value-bind (ivec l-iovecs-len l-iovecs) (ringbuf-iovec-info ringbuf)
    (ff-w/ivec (ivec-ptr ivec)
      (ccl:rlet ((iovecs (:array (struct :iovec) 2)))
        (loop 
           for i below l-iovecs-len
           for l-iovec = (aref l-iovecs i)
           for iovec = (ff-paref iovecs :iovec i)
           do (setf (ff-pref iovec :iovec.iov_base) (ff-inc ivec-ptr (aref l-iovec 0))
                    (ff-pref iovec :iovec.iov_len)  (aref l-iovec 1)))

        (let ((ret (#_readv fd iovecs l-iovecs-len)))
          (when (< 0 ret)
            (ringbuf-proceed ringbuf ret))
          (values ret (ff-errno))
          )))))

(def** sock-write! (fd sbuf)
  "update sbuf. -1:again(non-block). -2:error. -3:eof. n>=0:rest con-len"
  (ff-w/ivec (ptr (sbuf-ivec sbuf))
   (let* ((con-start (sbuf-con-start sbuf))
         (con-len (sbuf-con-len sbuf))
         (proceed-len (#_write fd (ff-inc ptr con-start) con-len)))
      ;;(logd ">>sock-write! proceed-len : ~s, con-len:~s" proceed-len con-len)
      (cond
        ((eql proceed-len -1)
         (let ((errno (ff-errno)))
           (ef (eql #$EAGAIN errno) ;; CCL-FFI ERROR? #$EAGAIN
               -1
             (values -2 errno))))
        ((eql proceed-len 0) ;; EOF
         (logd "@sock-write! EOF. con-len:~a" con-len)
         -3
         )
        (t
         (sbuf-con-proceed sbuf proceed-len)
         )))))

(def %sock-writev (fd ringbuf) ;;  ivec l-iovecs &aux (l-iovecs-len (length l-iovecs)))
  (multiple-value-bind (ivec l-iovecs-len l-iovecs) (ringbuf-iovec-info ringbuf :readp nil)
    (ff-w/ivec (ivec-ptr ivec)
      (ccl:rlet ((iovecs (:array (struct :iovec) 2)))
        (loop 
           for i below l-iovecs-len
           for l-iovec = (aref l-iovecs i)
           and iovec = (ff-paref iovecs :iovec i)
           do 
           (logd ">>~a ~a ~a" i (aref l-iovec 0) (aref l-iovec 1))
           (setf (ff-pref iovec :iovec.iov_base) (ff-inc ivec-ptr (aref l-iovec 0))
                 (ff-pref iovec :iovec.iov_len)  (aref l-iovec 1)))
        
        (let ((ret (#_writev fd iovecs l-iovecs-len)))
          (when (< 0 ret)
            (ringbuf-proceed ringbuf ret :readp nil))
          ret
          )))))

