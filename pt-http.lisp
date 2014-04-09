(in-package 'n.et)

#| http |#
(defparameter http-header-nl (coerce #(13 10) 'bytes))
(defparameter http-header-nl-txt (o2t http-header-nl))
(defparameter http-header-nl-len 2)
(defparameter http-header-delim (coerce #(13 10 13 10) 'bytes))
(defparameter http-header-delim-len 4)
(defparameter http-header-nil (t2o ""))

(defstruct http-stat
  (method)
  (path)

  (content-type-in) ;; content-type of request-telegram received. (I use the same field for content-length because there is little risk to confuse IN or OUT.)
  (params-txt) ;; query-string or post-parameter
  (params) ;; binary data if binary post parameter such as jpeg data. hash table if GET, POST-form, JSON

  (origin)
  (connection-close)

  (response-code)
  (content-type)
  (content)
  (content-length) ;; req resp. both use.

  (cookie)
  (set-cookie)

  (referer)
  (user-agent)
  (accept)
  (host)

  (out-params) ;; display param

  (location)

  (fd 0 :type (byte 32))
  (debug-info)
  )

(def** http-stat-gen ()
  (make-http-stat 
   :path "/" 
   :params (ht-gen :test 'equal)
   :out-params (ht-gen :test 'equal)
   :content-length 0
   :response-code 200
   :set-cookie (ht-gen :test 'equal)
   ))
   ;;:content-type ""))

(defmacro w/http-stat (st &body body)
  `(macrolet ((hs (&optional name set-obj)
                 (if name
                     (if set-obj
                         `(setf (,(intern (mkstr "HTTP-STAT-" name) :n.et) ,',st) ,set-obj)
                       `(,(intern (mkstr "HTTP-STAT-" name) :n.et) ,',st))
                   `,',st)))
    ,@body))

(defmacro! http-stat-case-set (st (&rest accessors) o!k o!v)
  `(cond
     ,@(mapcar (lambda (a1)
                 `((equal ,g!k ,a1 )
                   (setf (,(intern (mkstr "HTTP-STAT-" (string-upcase a1)) :n.et) ,st) ,g!v)
                   t))
               accessors)
     (t nil)))

;;(defmacro http-rewrite-hook (stat)
;;  "define and compile kql-handle-http"
;;  )

(def* http-cookie-val (val &key expires path domain secure http-only &aux (res val))
  (when expires
    (setq res (cat 'string res "; Expires=" (rfc-1123-date expires))))
  (when path
    (setq res (cat 'string res "; Path=" path)))
  
  res)
;; cookie
(def* http-cookie-parse (txt &aux (res (ht-gen :test 'equal)))
  (r:do-scans2 (#"r:([^=]+)=([^;]+)(; )?"# txt)
    (ht res (r:$ 0) (r:$ 1)))
  res)

(let ((reg-handle-header (r:r (concatenate 'string "([^:]+): ([^" #(#\cr)  "]+)" #(#\cr #\lf))))
      (reg-first (r:r (concatenate 'string "([^ ]+) ([^ ]+)(?: ([^ ]+))" #(#\cr #\lf))))
      (reg-header (r:r #"q:([^?]+)\?(.*)"#))
      )
  (def** http-handle-header (stat ivec start end)
    (w/http-stat stat
     (let ((txt (o2t ivec :start (or start 0) :end end)))
       (r:rif (reg-first txt) ;; first line
        (progn
          (hs :method (intern (string-upcase (r:$ 0)) :keyword))
          (hs :path (r:$ 1))
          (setq start (r:$ :mt)))
        (error "header parse error. txt:~a" txt))

       ;; header
       (r:do-scans2 (reg-handle-header txt :start start)
         (let ((k (r:$ 0))
               (v (r:$ 1)))
           (unless (http-stat-case-set ;;normal
                    stat
                    ("Host" "Referer" "User-Agent" "Content-Type")
                    k v)
             (unless (http-stat-case-set
                      stat
                      ("Content-Length")
                      k (parse-integer v :junk-allowed t))
               (http-stat-case-set
                stat
                ("Cookie")
                k (http-cookie-parse v)))
             ))))

     ;; copy content-type to content-type-in not to confuse.
     (hs :content-type-in (hs :content-type))

     (let ((path (hs :path)))
       (case (hs :method)
         (:get
          (r:rif (reg-header path)
           (progn
             (hs :path (r:$ 0)) ;; update path.
             (when (r:$ 1)
               (hs :params-txt (url-decode (r:$ 1))))))
           
          (let ((params (hs :params)))
            (r:do-scans2 (#"r:([^=]+)=([^&]+)&?"# (hs :params-txt))
              (let ((k (r:$ 0))
                    (v (r:$ 1)))
                (ht params k v)
                ))))
          
         (:post
          ;;do nothing.
          )))

     (values))))

(eval-always
  (defvar http-procs (ht-gen :test 'equal))
  (def http-proc-find (path)
    (aif (ht http-procs path)
        it
      (ht http-procs "/404.html")))

  (defmacro! http-proc-reg (path o!fn)
    `(progn
       (setf (symbol-function 
              (intern (string-upcase (mkstr "http-proc-fn-" ,path)) :n.et))
             ,g!fn) ;; automatic define function.
       (ht http-procs ,path ,g!fn)))

  (defmacro! http-proc-def (path &body body)
    `(http-proc-reg
      ,path
      (-l (,g!stat)
        (w/http-stat ,g!stat
          ,@body))))

  (defmacro http-def-stat ((path &key content-type) &body content)
    (unless content-type
      (setq content-type "text/html; charset=utf-8"))
    `(http-proc-def ,path
      (hs :content-type ,content-type)
      (hs :content ,@content)
      (logd ">>>>DUMDUM3")
      (-vals :dum)))

  (defmacro http-def-stat-js (path &body body)
    `(http-def-stat (,path :content-type "application/javascript")
       (w/$2o 
         ($js-raw ,@body))))
         ;; (ps:ps-to-stream *standard-output*
         ;;   ,@body))))
  (defmacro http-def-stat-html (path &body body)
    `(http-def-stat (,path)
       (w/$2o
         ,@body)))
  (defmacro http-def-stat-json (path obj)
    `(http-def-stat (,path :content-type "application/json")
       (w/$2o
         (yason:encode ,obj))))

  (defparameter http-response-codes (ht-gen))
  (ht http-response-codes 
      200 (t2o "200 OK")
      301 (t2o "301 Moved Permanently")
      302 (t2o "302 Found")
      404 (t2o "404 Not found")
      500 (t2o "500 Internal error"))
  (defparameter http-ver (t2o "HTTP/1.1 "))
  )

(def** http-resp-buf-gen (stat)
  (w/http-stat stat
   (let* ((cont (hs :content))
          (resp-code (ht http-response-codes (hs :response-code)))
          (cont-type (t2o (hs :content-type)))
          (set-cookie (t2o
                       (multiple-value-bind (k v) (s:scan-hash
                                                   (hs :set-cookie))
                         (s:collect-fn 'string
                          (l() "")
                          (l(acm a b)
                           (format nil "~aSet-Cookie: ~a=~a~a" acm a b http-header-nl-txt))
                          k v))
                       :lt :unix
                       ))
          (location (if (hs :location)
                        (t2o (format nil "Location: ~a~a" (hs :location) http-header-nl-txt) :lt :unix)
                      http-header-nil))
          (cont-len (t2o (format nil "Content-Length: ~a" (length cont)))))

     (sbuf-gen
      (bytes-cat http-ver resp-code http-header-nl

       (t2o "Content-Type: ") cont-type http-header-nl
                  
       (ef (hs :connection-close)
           (t2o "Connection: close")
         ;;(t2o "Connection: close"))
         (t2o "Connection: Keep-Alive"))
       http-header-nl ;; test

       set-cookie

       location

       cont-len 
       http-header-delim

       cont)))))

(-def http-handle-body (con stat)
 (w/http-stat stat
  (ef (= (hs :content-length) 0)
      (-vals nil)
      
    (-bind (ringbuf) (io-read-len con (hs :content-length) nil)
     (logd "@http-handle-body. >>>> ringbuf:~s" ringbuf)
     (multiple-value-bind (ivec start len) (ringbuf2ivec ringbuf)
       ;; post param read and set.
       (logd ">>>> POST:~s" (o2t ivec :start start :end (+ start len)))
       (logd ">>>> receive content-type:~s" (hs :content-type))

       (let ((ct (hs :content-type)))
         (cond ;; TORO:trie-tree for performance.
           ((string-equal ct "application/json" :start1 0 :end1 16 :start2 0 :end2 16)
            (hs :params (yason:parse (o2t ivec :start start :end (+ start len))))
            (logd ">>>josn:~s" (hs :params))
            )
           ((string-equal ct "application/x-www-form-urlencoded" :start1 0 :end1 33 :start2 0 :end2 33)
            ;; post by form submit
            (hs :params-txt ;; TODO:binay
             (url-decode
              (o2t ivec :start start :end (+ start len))))

            (let ((params (hs :params))) ;; same as url-parameter analyze when GET.
              (r:do-scans2 (#"r:([^=]+)=([^&]+)&?"# (hs :params-txt))
                (let ((k (r:$ 0))
                      (v (r:$ 1)))
                  (ht params k v)
                  ))))
           (t ;; other case set binary-data to params
            (hs :params (subseq ivec start (+ start len))))
           )))
     (-vals t)))))

(define-condition http-cond-500 (simple-error)
  ()
  (:report "condition. http-cond-500"))

(def* vp-gen-http-handler (nfd &aux con (age 0) tmp)
  "generate http-handler vp."

  (logd ">>>> vp-gen-httpd-handler called 1")

  (setq con (kq-con-gen nfd :rbuf (ringbuf-gen 512)))

  (logd ">>>> vp-gen-httpd-handler called 2")

  (alambda* (&aux stat proc-id)
    (logd ">>> vp-http-handler called 1")
    (setf stat (http-stat-gen))
    (w/http-stat stat
     (hs :fd (con-fd con))

     (-cs/hdl
      (-binds
       ((ringbuf)
        (io-read-until con http-header-delim 60000)) ;; 60sec

       ((res)
        (con-rbuf-proceed con http-header-delim-len :con-len-p nil) ;; read skip
        (multiple-value-bind (ivec start len) (ringbuf2ivec ringbuf (+ (ringbuf-con-len ringbuf) http-header-delim-len))
          (http-handle-header stat ivec start (+ start len))
          (ef (aand (hs :content-length)
               (< #.(* 128 1024 1024) it))
              (-signal 'http-cond-500)
            (http-handle-body con stat))))

       ((dum)
        (hs :content-type "text/html; charset=utf-8") ;; initialize.
        (let ((proc (http-proc-find (hs :path)))) ;; proc defined by user.
          (-fc proc stat)))
         
       ((dum)
        (con-write con (http-resp-buf-gen stat))
        (io-write-finish con)
        (logd "http con wrote")
        )

       (progn ;; yield
         (incf age)
         (ef (< 5 age)
             (progn
               (setq age 0)
               (vp-suspend #'self))
           (self))))
      (http-cond-500
       (c) 
       (logd ">>>>> 500 ERROR")
       (http-resp-500 stat con (format nil "c:~s(~a)" c c)))
      (error 
       (c) 
       (con-close con) ;; close
       (logd ">>> ERROR@handle-http-gened c:~s(~a)" c c))))))

(def** http-nfd-handler (nfd &aux ncon )
  "when accept return nfd, spawn vp for http"
  #+freebsd
  (let ((afa_size_bits (ff-size :accept_filter_arg :bits)))
    (ff-w/txts ((httpready "httpready"))
     (ccl:rletz ((afa :accept_filter_arg))
      (setf (ff-pref afa :accept_filter_arg.af_name) httpready)
      (#_setsockopt nfd 
       #$SOL_SOCKET
       #$SO_ACCEPTFILTER
       afa
       afa_size_bits)))
    )

  (logd ">>> http-nfd-handler called")
  ;; reg http proc
  (let ((vp-http
         (vp-gen-http-handler nfd)
         ))
    (logd ">>http-nfd-handler suspend!")
    (vp-suspend vp-http)
    ))

;;;; system default data.

(defparameter http-dat-top (merge-pathnames "workbox/dev/n/src/et/dat/" (probe-file (ccl:getenv "HOME"))))



(let ((favicon-oct (ccl:map-file-to-octet-vector (merge-pathnames "favicon.ico" http-dat-top))))
  (http-proc-def "/favicon.ico"
   (hs :content favicon-oct)
   (hs :content-type "application/octet-stream")
   (logd ">>>>DUMDUM1")
   (-vals :dum)))

(http-proc-def "/404.html"
 (hs :response-code 404)
 (hs :content
  (w/$2o
   ($html
    (:head
     (:title "miku-db 404"))
    (:body
     (:br)
     ((:center :style "color:#008b8b;")
      (:h1 "404 - miku can not find content."))
     ((:center :style "color:#00ced1;")
      (:h1 "404 - miku can not find content."))
     ((:center :style "color:#40e0d0;")
      (:h1 "404 - miku can not find content."))
     ((:center :style "color:#c71585;")
      (:h1 "PATH:" (hs :path)))
     ))))
 (logd ">>>>DUMDUM2")
 (-vals :dum))

(def* http-resp-500 (stat con info) ;; TODO:delete
  (logw "HTTP 500 response info:~s" info)
  (w/http-stat stat
    (hs :response-code 500)
    (hs :connection-close t)
    (hs :content 
        (w/$2o
          ($html
            (:head
             (:title "miku-db 500"))
            (:body
             (:br)
             ((:center :style "color:#008b8b;")
              (:h1 "500 - miku can not handle request."))
             ((:center :style "color:#00ced1;")
              (:h1 "500 - miku can not handle request."))
             ((:center :style "color:#40e0d0;")
              (:h1 "500 - miku can not handle request."))
             ((:center :style "color:#c71585;")
              (:h1 "PATH:" (hs :path)))
             ((:center :style "color:#c71585;")
              (:h1 "INFO:" info))
             ))))
    (con-write con (http-resp-buf-gen stat))
    (-cs/hdl
     (-bind (dum) (io-write-finish con)
       ;;(declare (ignore dum))
       (kq-con-close con))
     (error 
      (c) nil) ;; TODO: define "-ignore-errors"
     )))

