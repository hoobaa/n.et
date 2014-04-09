(asdf:defsystem :n.et
    :description "network. kqueue, socket, etc."
    :author "d.n. <strobolights@gmail.com>"
    :version 0.1
    :depends-on ( :nnl :parenscript :uuid :yason ) ;; :n.proc )
    :components
    ((:file "pkg")

     (:file "base" :depends-on ("pkg"))

     (:file "util" :depends-on ("base"))

     (:file "socket" :depends-on ("util"))
     (:file "con" :depends-on ("socket"))

     (:file "io" :depends-on ("util" "con"))

     (:file "pt-cass-util" :depends-on ("pkg"))
     (:file "pt-cass" :depends-on ("io" "pt-cass-util"))

     (:file "fmt-ml" :depends-on ("pkg" "pt-http-util"))

     (:file "pt-http-util" :depends-on ("util"))
     (:file "pt-http" :depends-on ("util" "con" "io" "fmt-ml" "pt-http-util"))

     ;;(:file "fmt-miku" :depends-on ("pkg"))
     (:file "fmt-js" :depends-on ("pkg"))
     ))
  
