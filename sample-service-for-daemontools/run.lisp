(pushnew "/Users/strobolights/workbox/dev/n.et/" asdf:*central-registry* :test #'equal)

(require 'swank)
(swank:create-server :port 4002 :dont-close t)

(format t ">>>> swank started~%")

(require 'n.et)
(in-package :n.et)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test page.
(http-proc-def "/demo"
 (hs :content
  (w/$2o
   ($html
    (:head
     (:title "m"))
    (:body
     (:h1 "demo on n.et")
     
     (:ol
      (:li ((:a :href "demo-static") "static"))
      (:li ((:a :href "demo-js.js") "js"))
      (:li ((:a :href "demo-html-and-js") "html-and-js"))
      (:li ((:a :href "demo-k") "continuation"))
      (:li ((:a :href "mkl") "miku-log"))
      )))))
 (-vals :dum))

(http-proc-def "/demo-static"
 (hs :content
  (w/$2o
   ($html
    (:body
     (:h1 "HELLO WORLD")
     ((:div :id "d1")
      "Google link is " ((:a :href "http://www.google.com") "here."))
     ((:div :id "d1" :style ($style (:color ($col :pink 5))))
      "Div with style.")))))
 (-vals :dum))

(http-proc-def "/demo-js.js"
 (hs :content-type "application/javascript")
 (hs :content
  (w/$2o
   (ps:ps-to-stream *standard-output*
    (defvar n (ht))
    (setf n.test 
          (lambda ()
            (alert "HELLO")))
    )))
 (-vals :dum))

(http-proc-def "/demo-html-and-js"
 (hs :content
  (w/$2o
   ($html
    (:head
     ($js-link "/demo-js.js")
     ($js-link "http://ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js")
     ($js-inline
      ((ps:@ ($ "#test") live) "click" (ps:@ n "test"))
      ))
    (:body
     ((:div :id "test" 
       :style ($style 
               (:text-align "center")
               (:background-color "pink") 
               (:color "#555555")))
      "test")))))
 (-vals :dum))




(kq-init)

(format t ">>>> kq inited~%")

(let ((lfd (sock-listen #(0 0 0 0) 11080)))
  (vp-enq (vp-gen-acceptor lfd #'http-nfd-handler)))

(format t ">>>> http acceptor started~%")

(format t ">>>> kq-loop will start~%")
(kq-loop)
