(in-package 'n.et)

;; util

(defmacro def (spec args &body body)
  `(defun ,spec ,args
     ,@body))

(defmacro def* (spec args &body body)
  `(defun ,spec ,args
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     ,@body))

(defmacro def** (spec args &body body) ;; inline
  `(progn
    ;;(declaim (inline ,spec))
     (def* ,spec ,args ,@body)))

(defmacro l (args &body body )
  (if (integerp args)
      `(lambda ,(loop for i below args
                   collect (symb "A" i))
         ,@body)
      `(lambda ,args ,@body)))
(defmacro l* (args &body body)
  `(l ,args
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     ,@body))

(defmacro alambda* (parms &body body)
  `(labels ((self ,parms 
             #f
             ,@body))
    #f
    #'self))

(defmacro nick-fn (nick org-f)
  "gen function nickname"
  `(setf (fdefinition ',nick) (fdefinition ',org-f)))

(defmacro nick-mac (nick org)
  "depend on Clozure :( Portability?? I can eat it?"
  `(eval-always
     (ccl::record-arglist ',nick (format nil "~a" (ccl::%arglist ',org)))
     (setf (macro-function ',nick) (macro-function ',org))))


;; type
;;(deftype byte () '(unsigned-byte 8))
;;(deftype byte () '(unsigned-byte 8))
(deftype bytes () '(vector (unsigned-byte 8)))
(deftype byte (&optional (w 8))
  `(unsigned-byte ,w))
(deftype sbyte (&optional (w 8))
  `(signed-byte ,w))


;; string
(defmacro! enc-name-conv (o!enc-name)
  `(cond
     ((eq ,g!enc-name :utf8)
      :utf-8)
     (t ,g!enc-name)))

(defmacro! t2o ( txt &key (enc :utf-8) (lt :cr) (start 0) end vec (vec-offset 0) )
  `(let ((,g!enc (enc-name-conv ,enc)))
     (ccl:encode-string-to-octets ,txt
                                  :external-format (ccl:make-external-format :character-encoding ,g!enc :line-termination ,lt)
                                  :start ,start
                                  ,@(if end `(:end ,end))
                                  ,@(if vec
                                        `(:vector ,vec :vector-offset ,vec-offset))
                                  )))

(defmacro! t2o-len ( o!txt &key (enc :utf-8) (start 0) end )
  (setf enc (enc-name-conv enc))
  `(ccl::string-encoded-length-in-bytes
    (ccl:get-character-encoding ,enc)
    ,g!txt
    ,start
    ,(if end 
         end 
         `(- (length ,g!txt) ,start))))

(defmacro o2t (o &key (enc :utf-8) (lt :cr) (start 0) end )
  "enc is utf8, euc-jp, cp932"
  (setf enc (enc-name-conv enc))
  `(ccl:decode-string-from-octets (coerce ,o '(vector (unsigned-byte 8))) :external-format (ccl:make-external-format :character-encoding ,enc :line-termination ,lt) :start ,start ,@(if end `(:end ,end))  ))



;; hash-table
(defmacro ht ( tgt &rest kvs ) ;;  &key (test #'eql) (size 100) &allow-other-keys )
  (case (length kvs)
    (1 `(gethash ,(car kvs) ,tgt))
    (t
     `(progn
        ,@(mapcar
           #`(setf (gethash ,(car a1) ,tgt) ,(cadr a1))
           (group kvs 2))))))

(def** ht-gen (&key (test 'eql) (size 60) v hash-function)
  (let ((ht (make-hash-table :test test :size size :hash-function hash-function)))
    (s:iterate (((kk vv) (s:scan-plist v)))
        (setf (gethash kk ht) vv))
    ht))

(nick-fn ht-map maphash)
(nick-fn ht-clr clrhash)
(defmacro ht-rm (tgt k)
  `(remhash ,k ,tgt))


;; continuation
(defparameter _chdl2 nil)
(define-symbol-macro _chdl _chdl2)
(defparameter _ncont2 nil "nameed cont. used in nbinds.")
(define-symbol-macro _ncont _ncont2)

(defmacro -bind (args exp &body body)
  `(let ((_cont (l* (_chdl ,@args)
                ,@body
                )))
       ,exp))

(defmacro -binds (&body body) ;; TODO: context switch performance. is this light weight? maybe chdl is heavy.
  (let ((ta (car body))
      (td (cdr body)))
    ;;(g!chdl-bak (gensym)))
    (ef td
        `(let ((_cont (l* (_chdl ,@(car ta)) ;;;; !!
                      (-binds
                        ,@td)
                      )))
           ,@(cdr ta))
      ta)))

(defmacro -def (name args &body body) ;; using defmacro!, error...
  (let ((fn-name (symb :- name)))
    `(progn
       (eval-always
         (defmacro ,name ,args
           `(,',fn-name _cont _chdl ,,@args))
         )
       (eval-when (:load-toplevel :execute)
         (def* ,fn-name (_cont _chdl ,@args)
           ,@body)
         )
       )))

(defmacro -l (params &body body)
  `(l* (_cont _chdl ,@params) 
     ,@body))

(defmacro -vals (&rest retvals)
  `(funcall _cont _chdl ,@retvals))
;;(defmacro -vals (&rest retvals)
;;  `(w ((_chdl _chdl)) ;; call continuation with old chdl environment.
;;     (funcall _cont ,@retvals)))

(defmacro -fc (fn &rest args)
  `(funcall ,fn _cont _chdl ,@args))
(defmacro -apply (fn &rest args)
  `(apply ,fn _cont _chdl ,@args))


(defmacro -find-hdl (c)
  `(assoc ,c _chdl :test (l(k v) (typep k v))))

(defmacro -b/hdl (hdls &body body)
  "like handler-bind"
  (with-gensyms (g!chdl_bak)
    `(let ((,g!chdl_bak _chdl))
       
       ,@(mapcar (l(hdl)
                   `(push (cons ',(car hdl) ,(cadr hdl))  _chdl))
                 hdls)
       ,@body
       (setf _chdl ,g!chdl_bak))))

(defmacro -cs/hdl (expr &rest clauses)
  "like handler-case"
  `(-b/hdl ,(mapcar
             #`( ,(car a1) 
                  (l ,(cadr a1)
                    ,@(cddr a1)))
             (reverse clauses))
     ,expr))

(defmacro! -signal (o!c &rest rest)
  `(progn
     (typecase ,g!c
       (symbol (setf ,g!c (make-condition ,g!c ,@rest)))
       (t nil))
     (awhen (-find-hdl ,g!c)
       (funcll (cdr it) ,g!c))))

;; FFI

(defmacro ff-deftype (&rest specs)
 `(progn
   ,@(loop for (name type) on specs by #'cddr
           collect `(ccl:def-foreign-type ,name ,type))))

(eval-always
 (ff-deftype
  :ui8  (:unsigned 8)
  :ui16 (:unsigned 16)
  :ui32 (:unsigned 32)
  :ui64 (:unsigned 64)
  :ui128 (:unsigned 128)
  
  :i8 (:signed 8)
  :i16 (:signed 16)
  :i32 (:signed 32)
  :i64 (:signed 64)
  ;; :i128 (:signed 128)
  
  :f32 :float
  :f64 :double
  
  :ptr :macptr
  ))

(eval-always
 (defvar platform-sym 
     #+freebsd 'x86-freebsd64 
     #+linux 'X86-LINUX64 
     #+darwin 'x86-darwin64 )
 
 (def ff-load-c-fn (&rest syms)
   (dolist (sym syms)
     (ccl::load-external-function (intern sym platform-sym) nil)
     ))
 (def ff-load-c-constant (&rest syms)
   (dolist (sym syms)
     (ccl::load-os-constant (intern sym platform-sym) nil)
     ))
 (def ff-load-c-var (&rest syms)
   (dolist (sym syms)
     (ccl::%load-var (intern sym platform-sym) nil)
     ))
 (def ff-load-c-type (&rest types)
   "using macro or inline function, type error may occur. so use this function not to be error."
   (dolist (type types)
     (ccl::parse-foreign-type type)))
 
 
 (ff-load-c-fn "strerror" "memset" "getpid" "gettimeofday" "__error")
 )

(deftype ptr ()
  'macptr)

(nick-fn ff-so-open ccl:open-shared-library)
(nick-fn ff-so-close ccl:close-shared-library)

;; (defmacro ff-errno ()
;;  "unix external errno"
;;  `(ccl::%get-errno))
;; ;;`(ff-ref (#___error) :ui32))
(defun ff-errno () ;; TODO: macro or inline
  #+darwin(pref (#___error) :int)
  #+freebsd(values #&errno))

(defmacro! ff-errno2txt (errno)
 `(ff-2txt (#_strerror ,errno)))
(defmacro! ff-errno-by-txt ()
 `(let ((,g!errno (ff-errno)))
   (format nil "~a(~a)" ,g!errno (ff-2txt (#_strerror ,g!errno)))))

(eval-always
 (defvar ff-nil (ccl:%null-ptr))
 (nick-fn ff-null ccl:%null-ptr-p)
 
 (nick-fn ff-accessor ccl::%foreign-access-form)
 (nick-fn ff-array-accessor ccl::%foreign-array-access-form)
 
 
 (def l!-symbol-p (s) ;; lexical variable (not auto of C)
   (and (symbolp s)
        (> (length (symbol-name s)) 2)
        (string= (symbol-name s)
         "L!"
         :start1 0
         :end1 2)))
 )


;; (def** ff-ptr ()
;;   (ccl::make-gcable-macptr ccl::$flags_DisposPtr))
;; (defmacro ff-malloc (size &optional clean-p)
;;   `(ccl::%new-ptr ,size ,clean-p))
(nick-fn ff-malloc ccl::%new-ptr)

(defmacro ff-new (type &rest initforms)
 `(ccl::make-gcable-record ,type ,@initforms))

;;(nick-mac ff-new2 ccl::make-gcable-record)


#| array |#
(defmacro! ff-w/arrays (specs &body body)
 `(let* ( ,@(mapcar #`( ,(symb (car a1) "-SIZE") (* (ff-size ,(cadr a1)) ,(caddr a1)) ) specs )
       ,@(mapcar #`( ,(car a1) (ff-malloc ,(symb (car a1) "-SIZE") )) specs) )
   ,(let ((auto-vars (filter (lambda (a) (unless (l!-symbol-p (car a)) (car a))) specs)))
       `(declare (dynamic-extent ,@auto-vars)
         (type ptr ,@auto-vars)
         (unsettable ,@auto-vars)))
   ,@(filter (lambda (a) 
              (when (cdddr a) ;; 4th arg. memset zero.
                `(#_memset ,(car a) 0 ,(symb (car a) "-SIZE"))))
             specs
             )
   ,@body))

(defmacro! ff-anew (spec size &optional clean-p)
 `(ff-w/arrays ((,g!l!p ,spec ,size ,clean-p))
   ,g!l!p))

;; (defmacro! ff-new (type &rest initforms)
;;   `(ccl::make-gcable-record ,type ,@initforms))

(defmacro ff-copy (type src dest)
 `(ccl::copy-record ,type ,src ,dest))

(eval-always
 (nick-fn ff-type ccl::%foreign-type-or-record)
 (def** ff-size (type &optional (units :bytes))
   (ccl::%foreign-type-or-record-size type units))
 )

(defmacro ff-ref (ptr type &optional (bit-offset 0) (accessors nil))
 (ff-accessor ptr
  (ff-type type)
  bit-offset accessors))

(eval-always
 (defvar types '(:ui8 :ui16 :ui32 :ui64 :ui128
               :i8 :i16 :i32 :i64
               :f32 :f64
               :ptr))
 
 (macrolet ((defex () 
             `(progn
               (def ff-dyn-ref (ptr type &optional (bit-offset 0))
                 (case type
                   ,@(mapcar
                      #`(,a1 (ff-ref ptr ,a1 bit-offset))
                      types)))
               (def (setf ff-dyn-ref) (val ptr type &optional (bit-offset 0))
                 (case type
                   ,@(mapcar
                      #`(,a1 (setf (ff-ref ptr ,a1 bit-offset) val))
                      types))))))
  (defex)))

;;;;;; CCL wrap
(defmacro ff-pref (pointer accessors)
 `(pref ,pointer ,accessors))
(defmacro ff-paref (pointer type-name index)
 `(paref ,pointer ,type-name ,index))
;;;;;;; 
;; ;; (defmacro aref (pointer type-name index)
;; ;;   (let* ((*target-ftd* (backend-target-foreign-type-data *target-backend*)))
;; ;;     (%foreign-array-access-form  pointer (%foreign-type-or-record type-name) index)))
;; (defmacro ff-refa (ptr type i)
;;   (let* ((ccl::*target-ftd* (ccl::backend-target-foreign-type-data ccl::*target-backend*))) ;; ??????
;;     (ff-array-accessor ptr (ff-type type) i)))

(defmacro ff-defstruct (name &body members)
 `(ccl:def-foreign-type ,name
   (:struct ,name
    ,@members)))

(defmacro ff-2txt (ptr &optional len dest)
 (ef len
     `(ccl::%str-from-ptr ,ptr ,len ,@(if dest `(,dest) nil))
   `(ccl::%get-cstring ,ptr)))

;; (defmacro! ff-2utxt (ptr &optional (len nil))
;;   `(do* ((,g!end 0 (1+ ,g!end)))
;;         (,(if len
;;               `(<= ,len ,g!end)
;;               `(zerop (the 'byte (ccl::%get-unsigned-byte ,ptr ,g!end))))
;;          (w* ((,g!ulen (ccl::utf-8-length-of-memory-encoding ,ptr ,g!end 0))
;;               (,g!string (make-string ,g!ulen)))
;;            (ccl::utf-8-memory-decode ,ptr ,g!end 0 ,g!string)
;;            ,g!string))
;;     (declare (fixnum ,g!end))))
(nick-fn ff-2utxt ccl::%get-utf-8-cstring)

;;(defun %str-from-ptr (pointer len &optional (dest (make-string len)))
;;  (declare (fixnum len)
;;           (optimize (speed 3) (safety 0)))
;;  (dotimes (i len dest)
;;    (setf (%scharcode dest i) (%get-unsigned-byte pointer i))))
;;(defun %get-utf-8-cstring (pointer)
;;  (do* ((end 0 (1+ end)))
;;       ((zerop (the (unsigned-byte 8) (%get-unsigned-byte pointer end)))
;;        (let* ((len (utf-8-length-of-memory-encoding pointer end 0))
;;               (string (make-string len)))
;;          (utf-8-memory-decode pointer end 0 string)
;;          string))
;;    (declare (fixnum end))))


(defmacro! ff-txt2 (txt &optional ptr (nul-terminated t))
 (ef ptr
     `(ccl::%cstr-pointer ,txt ,ptr ,nul-terminated)
   `(let ((,g!ptr (ff-malloc (1+ (length ,txt)))))
     (ccl::%cstr-pointer ,txt ,g!ptr ,nul-terminated)
     ,g!ptr)))

(nick-mac ff-w/txts ccl:with-cstrs)
(nick-mac ff-w/utxts ccl::with-utf-8-cstrs)

;; (defmacro ff-w/txts (specs &body body)
;;   `(with-cstrs ,specs
;;      ,@body))
;; 
;; (defmacro ff-w/utxts (specs &body body)
;;   `(ccl::with-utf-8-cstr ,specs
;;      ,@body))

(nick-fn ff-inc  ccl:%inc-ptr)
;;(defmacro ff-incf (p &optional (by 1))
;;  `(ccl:%incf-ptr ,p ,by))
;;(defmacro ff-w/ivec ( (ptr ivec) &body body )
;;  `(ccl:with-pointer-to-ivector ( ,ptr ,ivec )
;;     ,@body))
(nick-mac ff-incf ccl:%incf-ptr)
(nick-mac ff-w/ivec ccl:with-pointer-to-ivector)

(nick-fn ff-heap-ivec-gen ccl:make-heap-ivector)

;;;; 
(defmacro ff-set-ptr-type (ptr type)
 `(ccl::%set-macptr-type 
   ,ptr 
   (ccl::foreign-type-ordinal 
    (load-time-value (ccl::%foreign-type-or-record ,type)))))

(defmacro coerce-bytes (oct)
  `(coerce ,oct 'bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; o2val val2o
(defmacro! o2val (oct type &key (start 0) (bigp t) )
 (ef bigp
     `(let* ((oct-rev 
            (reverse (subseq ,oct ,start (+ ,start ,(ff-size type))))))
       (ff-w/ivec (,g!ptr (coerce-bytes oct-rev))
        (ff-ref ,g!ptr ,type 0 nil)))
   
   `(ff-w/ivec (,g!ptr ,(coerce-bytes oct))
     (ff-ref ,g!ptr ,type (* 8 ,start) nil))))

(defmacro! val2o (val type &key (oct) (start 0) (bigp t))
 (let* ((type-size (ff-size type))
      (end (+ start type-size)))
  `(let ((,g!oct ,oct))
    ,(unless oct
             `(setf ,g!oct (make-array ,type-size :element-type 'byte)))
    
    ,(if (eql :ui128 type)
         `(loop for i below 16
           for j = 0 then (+ j 8)
           do (setf (aref ,g!oct i) (ldb (byte 8 j) ,val)))
         `(ff-w/ivec (,g!ptr ,g!oct)
           (setf (ff-ref ,g!ptr ,type ,(* 8 start) nil) 
                 ,val)))
    
    ,(when bigp ;; bugbug.?? if oct is exits. destructive is strange. TODO:
           `(replace 
             ,g!oct
             (reverse (subseq ,g!oct ,start ,end))
             :start1 ,start :end1 ,end
             :start2 0 :end2 ,type-size))
    ,g!oct)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; o2val val2o dynamic version.
(defmacro! o2val-dyn (oct type &key (start 0) (bigp t) )
 (ef bigp
     `(let* ((oct-rev 
            (reverse (subseq ,oct ,start (+ ,start (ff-size ,type))))))
       (ff-w/ivec (,g!ptr (coerce-bytes oct-rev))
        (ff-dyn-ref ,g!ptr ,type 0)))
   
   `(ff-w/ivec (,g!ptr ,(coerce-bytes oct))
     (ff-dyn-ref ,g!ptr ,type (* 8 ,start)))))

(defmacro! val2o-dyn (val type &key (oct) (start 0) (bigp t))
 `(let* ((,g!type-size (ff-size ,type))
       (,g!end (+ ,start ,g!type-size)))
   (let ((,g!oct ,oct))
    ,(unless oct
             `(setf ,g!oct (make-array ,g!type-size :element-type 'byte)))
    
    (if (eq :ui128 ,type)
        (loop for i below 16
              for j = 0 then (+ j 8)
              do (setf (aref ,g!oct i) (ldb (byte 8 j) ,val)))
      
      (ff-w/ivec (,g!ptr ,g!oct)
       (setf (ff-dyn-ref ,g!ptr ,type ,(* 8 start)) 
             ,val)))
    
    ,(when bigp
           `(replace 
             ,g!oct
             (reverse (subseq ,g!oct ,start ,g!end))
             :start1 ,start :end1 ,g!end
             :start2 0 :end2 ,g!type-size))
    ,g!oct)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; service

#| time |#
(def n-time ()
  (declare (optimize (speed 3) (safety 0)))
  (rletz ((tm :timeval))
   (#_gettimeofday tm ff-nil)
   (values (+ (pref tm :timeval.tv_sec) 2208988800)
    (pref tm :timeval.tv_usec))))
(def n-time-usec ()
  (declare (optimize (speed 3) (safety 0)))
  (rletz ((tm :timeval))
   (#_gettimeofday tm ff-nil)
   (+
    (* 1000000 (+ (pref tm :timeval.tv_sec) 2208988800))
    (pref tm :timeval.tv_usec))))
(def n-unixtime ()
  (declare (optimize (speed 3) (safety 0)))
  (rletz ((tm :timeval))
   (#_gettimeofday tm ff-nil)
   (values (+ (pref tm :timeval.tv_sec))
    (pref tm :timeval.tv_usec))))
(def n-unixtime-usec ()
  (declare (optimize (speed 3) (safety 0)))
  (rletz ((tm :timeval))
   (#_gettimeofday tm ff-nil)
   (+
    (* 1000000 (+ (pref tm :timeval.tv_sec)))
    (pref tm :timeval.tv_usec))))


#| proc |#
(eval-always
 (defparameter proc-cur-id (#_getpid)))

(def proc-cur-id ()
  "update"
  (setf proc-cur-id (#_getpid)))

;;(defmacro ff-fc (entry &rest args)
;;  `(ccl:ff-call ,entry ,@args))
(nick-mac ff-fc ccl:ff-call)

(defmacro ff-fc2 (name &rest args)
 `(ccl:external-call ,name ,@args))


(nick-fn ff-ent ccl:foreign-symbol-entry)
(nick-fn ff-ptr ccl:foreign-symbol-address)
(nick-fn ff-i2ptr ccl:%int-to-ptr)
(nick-fn ff-ptr2i ccl:%ptr-to-int)


(defmacro logd (&rest rest) ;; DEBUG
  `(progn
    (format t ,@rest)
    (write-char #\Newline))
  )

(defmacro logw (&rest rest) ;; TODO:DEL WARN
  `(progn
    (format t ,@rest)
    (write-char #\Newline))
  )

(defmacro logi (&rest rest) ;; INFO
  `(progn
    (format t ,@rest)
    (write-char #\Newline))
  )
(defmacro loge (&rest rest) ;; ERROR
  `(progn
    (format t ,@rest)
    (write-char #\Newline))
  )


