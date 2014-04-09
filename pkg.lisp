(in-package :cl-user)
(defpackage #:n.et
  (:nicknames #:m)
  (:use :common-lisp :nnl :ccl)
  ;;(:import-from :ps :@ :var)
  ;;(:import-from :ccl :rlet :rletz :pref)
  (:export 
   ;; socket.
   #:sock-listen

   ;; kqueue, kevent
   #:kq-loop #:kq-init
   #:kq-fin

   ;; virtual process driven by kqueue. made by closure and Paul Graham's continuation macros.
   #:vp-gen-acceptor
   #:vp-enq

   ;; generate http vp handler. you pass this handler to vp-gen-acceptor.
   #:http-nfd-handler

   ;; http web server 
   #:http-proc-reg #:http-proc-def
   #:http-def-stat #:http-def-stat-js #:http-def-stat-html #:http-def--stat-json
   #:hs
   #:http-cookie-val

   ;; parenscript. javascript by s-expressions.
   #:js-mac 
   #:@ #:var #:ht #:ar #:l #:w #:w* #:aif #:aand #:gen #:mc #:n-log #:n=bind #:n=vals

   ;; HTML formatter.
   #:$ 
   #:$html #:$cdata #:$lisp #:$lisp1
   #:$js-link #:$js-inline #:$js-raw
   #:$p-esc
   #:w/$ #:w/$2o 
   #:html-esc

   #:$col
   #:$style
   #:$css

   #:gen-rss-bin #:make-rss-item

   #:http-cond-500

   ;; to communicate with cassandra
   #:cass-gen
   #:cass-release-con #:cass-get-con 
   #:cass-req 
   #:cass-req-get #:cass-req-insert
   #:w/cass-con
   #:cass-get-bare-con
   ))
