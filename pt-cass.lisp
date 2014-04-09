(in-package 'n.et)

;;;; pool
(defstruct cass 
  (server-ip   nil :type (vector (ubyte 8) 4))
  (server-port 0   :type (ubyte 32))

  (con-pool (ht-gen :test 'equal) :type hash-table) ;; <ks_name> => con list
  )

(def cass-gen (ip port)
  (make-cass :server-ip ip :server-port port))

;;;; api
(-def cass-req (con msg-bin)
  "request message and receive response."
  (let ((msg-len (length msg-bin)))
    (con-write con (sbuf-gen msg-bin))
    (-binds
      ((dum)
       (io-write-finish con))
      ((ringbuf) 
       (io-read-len con 4 nil))
      ((ringbuf)
       (multiple-value-bind (ivec start len) (ringbuf2ivec ringbuf)
         (let ((msg-len (o2val ivec :i32 :start start)))
           (io-read-len con msg-len nil))))
      (progn
        (multiple-value-bind (ivec start len) (ringbuf2ivec ringbuf)
         (let ((cass-is-bytes ivec)
              (cass-is-idx start))
            (w/r-msg (res-name res-ver res-seqid)
              (-vals (cass-read-auto)))))))))

(-def cass-get-con (cass ks)
  (let (con)
    (tagbody
     :loop
       (setq con (pop (ht (cass-con-pool cass) ks)))
       (cond
         ((and con (con-closed-p con)) ;; found and closed
          (logd ">>>cass-get-con. closed socket found. next loop.")
          (go :loop))

         ((not con)
          (multiple-value-bind (fd errno) (sock-con (cass-server-ip cass) (cass-server-port cass))
            (ef (eql -1 fd)
                (-signal (make-condition 'sock-creation-error :errno errno))
              (setq con (kq-con-gen fd :rbuf (ringbuf-gen 1024)))
              (-bind (ret) (cass-req con (pack-message (|MAKE-set_keyspace| :keyspace ks)))
                                     ;;(cass-req-set-keyspace ks))
                (-vals con)))))

         (t ;; found con from pool
          (logd ">>>FOUND-cass-con: fd:~s" (con-fd con))
          (-vals con))))))

(-def cass-get-bare-con (cass)
  (multiple-value-bind (fd errno) (sock-con (cass-server-ip cass) (cass-server-port cass))
    (ef (eql -1 fd)
        (-signal (make-condition 'sock-creation-error :errno errno))
      (-vals (kq-con-gen fd :rbuf (ringbuf-gen 1024))))))



(def cass-release-con (cass ks cass-con)
  (push cass-con (ht (cass-con-pool cass) ks)))

(defmacro w/cass-con ((cass con ks) &body body)
  `(symbol-macrolet ((release (m:cass-release-con ,cass ,ks ,con)))
     (-binds
       ((,con) (m:cass-get-con ,cass ,ks))
       ,@body)))
                       

