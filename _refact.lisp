(setf debug nil)
(dolist (path (directory "/Users/strobolights/workbox/dev/n.et/*.lisp"))
  (unless (equal (pathname-name path) "_refact")
    (let ((cont (s:collect 'string (s:scan-file path #'read-char)))
          cont2)
      (setf cont2 cont)
      (setf cont2 (#"sg:\(fmt :(format "# cont2))
      (setf cont2 (#"sg:\(fc :(funcall "# cont2))
      (setf cont2 (#"sg:\(almd :(alambda "# cont2))
      (setf cont2 (#"sg:\(almd\* :(alambda* "# cont2))
      (setf cont2 (#"sg:\(mv/b:(multiple-value-bind"# cont2))

      (with-open-file (os path :direction :output :if-exists :supersede)
        (princ cont2 os)))))

    ;;(push cont2 debug)))

