(in-package :n.et)

(defmacro js-mac (name args &body body)
  `(ps:defjsmacro ,name ,args
     ,@body))

(js-mac ht (&rest rest)
  `(ps:create ,@rest))
(js-mac ar (&rest rest)
  `(ps:array ,@rest))
(js-mac def (name args &body body)
  (if (listp name)
      `(setf (@ ,(car name) ,(cadr name))
             (l ,args ,@body))
      `(defun ,name ,args 
         ,@body)))
(js-mac l (args &body body)
  `(lambda ,args ,@body))
(js-mac w ( &rest rest )
  `(let ,@rest))
(js-mac w* ( &rest rest )
  `(let* ,@rest))
(js-mac aif (test a &optional b)
  `(let ((it ,test))
     (if it
         ,a
         ,b)))
(js-mac aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(js-mac gen (fname &rest rest)
  `((new ,fname) ,@rest))
(js-mac mc (meth obj &rest args) ;; method call
  `((@ ,obj ,meth) ,@args))

(js-mac n-log (&rest rest)
  `(console.log ,@rest))

;; continuation
(js-mac n=bind (args expr &body rest)
  `(let ((_cont (l ,args
                  ,@rest)))
     ,expr))
(defmacro n=bind (args expr &body rest)
  (error "dummy")
  )
(js-mac n=vals (&rest vals)
  `(_cont ,@vals))
