(in-package 'n.et)

;;;;;;;;;; HTML
(eval-always 
  (def $-ex-args (args)
    `(progn
       ,@(s:collect
          (s:mapping (((k v) (s:chunk 2 2 (s:scan args))))
                     `(progn
                        (write-char #\Space)
                        (princ ,(let ((nm (symbol-name k)))
                                   (if (char= (char nm 0) #\*)
                                       (string-downcase (subseq nm 1))
                                       nm)))
                        (princ "='")

                        ,(let ((exp v))
                            (cond
                              ((and (listp exp) (eql (car exp) '$style))
                               exp)
                              (t
                               `(princ ,exp))))

                        (write-char #\'))))))

  (def $-sym-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 0)
         (char= #\$ (char (symbol-name s) 0))))

  (def $-ex-elems (elems)
    `(progn
       ,@(mapcar
          (l(elem)
            (ef (listp elem)
                (cond
                  ((eq '$lisp (car elem))
                   `(progn ,@(cdr elem)))
                  ((eq '$lisp1 (car elem))
                   (cdr elem))
                  (($-sym-p (car elem))
                   elem)
                  ((or (and (listp (car elem))
                            (keywordp (caar elem)))
                       (keywordp (car elem)))
                   (cons '$ elem))
                  (t
                   `(princ ,elem)))
              `(princ ,elem)))
          elems)))
  )

(defmacro $ (spec &body body &aux name)
  "tag generator. string version."

  (let ((name-sym (if (symbolp spec) spec (car spec))))
    (ef (keywordp name-sym)
        ;;(setq name (string-downcase (sym-name name-sym)))
        (setq name
              (let ((na (symbol-name name-sym)))
                (if (char-equal (char na 0) #\*) ;; first character is * , then downcase.
                    (string-downcase (subseq na 1))
                    na)))
      (setq name (format nil "~a" name-sym))))
      
  `(progn
     ,(ef (symbolp spec)
          `(progn 
             (write-char #\<)
             (princ ,name))
          `(progn 
             (write-char #\<)
             (princ ,name)
             ,($-ex-args (cdr spec))))
     ,(ef body
          `(progn
             (write-char #\>)
             ,($-ex-elems body)
             (princ "</")
             (princ ,name)
             (write-char #\>))
          
          `(princ "/>"))))

;; (mac $progn (&body body)
;;   "tag generator. string version."
;;   `(progn
;;      ,($-ex-elems body)))

(defmacro $html (&body body)
  `(progn
     (format t "<!DOCTYPE html>")
     ($ (:html :xmlns "http://www.w3.org/1999/xhtml")
       ,@body)))

(defmacro $cdata (&body body)
  `(progn
     (princ "<![CDATA[")
     ,@body
     (princ "]]>")))

(defmacro $js-link (src)
  `($ (:script :src ,src :type "text/javascript") ""))
(defmacro $js-inline (&body body)
  `($ (:script :type "text/javascript") ;; PS is Best??
     ($lisp1 ps:ps-to-stream *standard-output*
             ,@body)))
(defmacro $js-raw (&body body)
  `(ps:ps-to-stream *standard-output*
     ,@body))


;;(defmacro $p ( obj )
;;  `(princ ,obj))
(defmacro $p-esc (&rest objs)
  `(princ (html-esc (mkstr ,@objs))))

(defmacro! w/$ (&body body)
  `(with-output-to-string (,g!os)
     (let ((*standard-output* ,g!os))
       ,@body)))
(defmacro! w/$2o (&body body)
  `(t2o
    (w/$ 
      ,@body)))

;;;;;; CSS
(eval-always
  (def $css-style-exp (stys)
    `(progn
       ,@(mapcar
          #`(progn
              (princ ,(if (symbolp (car a1))
                          (string-downcase (symbol-name (car a1)))
                          (car a1)))
              (write-char #\:)
              (princ ,(cadr a1)) ;; first elem
              ,@(mapcar #`(progn
                            (write-char #\space)
                            (princ ,a1))
                        (cddr a1))
              (write-char #\;))
          stys))))

(defmacro $css (&rest rest)
  #"q: ex)
  ($css 
    ("div" (:color "red") (:align "left")))
  "#
  `(progn
     ,@(mapcar 
        (l(a &aux dispatcher)
          (when (listp a)
            (setq dispatcher (car a)))
          (case dispatcher
            ('$lisp
             `(progn
                ,@(cdr a)))
            ('$lisp1 ;; 1 liner
             `(cdr a))
            (t
             `(progn
                (princ ,(car a))
                (princ " {")
                ,($css-style-exp (cdr a))
                (format t "}~%")))))
        rest)))

(defmacro $style (&rest rest)
  ($css-style-exp rest))

;;;;;; RSS1.0
(defstruct rss-item 
  (about)

  (title)
  (link)
  (description)
  (dc-subject)
  (dc-date)
  (dc-creator)
  (content))

(def** fmt-time (utime)
  (multiple-value-bind (s m h dd mm yy dow dlp tz) (decode-universal-time utime -9)
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d+09:00" yy mm dd h m s)))
    
(def* gen-rss-bin (title link description items)
  (w/$2o
    (princ "<?xml version='1.0' encoding='UTF-8'?>")
    ($ (:|rdf:RDF|
         :*xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         :*xmlns:dc "http://purl.org/dc/elements/1.1/"
         :*xmlns:admin "http://webns.net/mvcb/"
         :*xmlns:content "http://purl.org/rss/1.0/modules/content/"
         :*xmlns "http://purl.org/rss/1.0/")

      ((:*channel :*rdf:about link)
       (:*title title)
       (:*link link)
       (:*description description)

       (:*items
         (:|rdf:Seq|
           ($lisp1 dolist (item items)
                   ($ (:*rdf:li :*rdf:resource (rss-item-about item))))
           )))
      ($lisp1 dolist (item items)
              ($ (:*item :*rdf:about (rss-item-about item))
                (:*title (rss-item-title item))
                (:*link (rss-item-link item))
                (:*description (rss-item-description item))
                (:*dc:subject (rss-item-dc-subject item))
                (:*dc:date (fmt-time (rss-item-dc-date item)))
                (:*dc:creator (rss-item-dc-creator item))
                (:*content:encoded (rss-item-content item)))))))

;;;; site custom
(eval-always
  (def base-vec (rgb &aux len)
    (setq len
          (loop for i below 3
             sum (aref rgb i)))
    (map '(simple-vector 3) (lambda (x) (/ x len)) rgb))

  (def col-vec (base-vec deg)
    (map '(simple-vector 3)
         (lambda (x)
           (min 
            255
            (floor
             (* (/ (* 3 255) 10) deg x))))
         base-vec))
  
  (defparameter color-tab (ht-gen))

  (ht color-tab 
      :green (base-vec #(#xaf #xee #xee))
      :blue  (base-vec #(#x1e #xb7 #xc9))
      :pink  (base-vec #(#xe1 #x28 #x85))
      :mono  (base-vec #(1 1 1))
      :negi  (base-vec #(#xad #xf6 #xb0))
      :skin  (base-vec #(#xfa #xe2 #xbc)))

  (defmacro $col-vec (name degree)
    `(aif (ht color-tab ,name)
         (col-vec it ,degree)
       (col-vec (ht color-tab :mono) ,degree)))

  (defmacro! $col (name degree)
    (ef (and (keywordp name) (integerp degree))
        (let ((cv ($col-vec name degree)))
          (format nil "#~2,'0x~2,'0x~2,'0x" (aref cv 0) (aref cv 1) (aref cv 2)))

      `(let ((,g!cv ($col-vec ,name ,degree)))
         (format nil "#~2,'0x~2,'0x~2,'0x" (aref ,g!cv 0) (aref ,g!cv 1) (aref ,g!cv 2)))))
  )
