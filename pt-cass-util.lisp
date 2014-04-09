(in-package :n.et)

(eval-always
  (defvar ttype-sym2id (ht-gen))
  (defvar ttype-id2sym (ht-gen))
  (defvar ttype-id2size (ht-gen))

  (def cass-def-type (id sym size-exp)
    (ef (atom sym)
        (progn
          (ht ttype-sym2id sym id)
          (ht ttype-id2sym id sym))
      (dolist (sy sym)
        (ht ttype-sym2id sy id)
        (ht ttype-id2sym id sy)))

    (ht ttype-id2size id
        size-exp))

  (cass-def-type 0 :stop 1)
  (cass-def-type 1 :void 1)
  (cass-def-type 2 :bool 1)
  (cass-def-type 3 '(:byte :i08 :i8) 1)
  (cass-def-type 4 :double 8)
  (cass-def-type 6 :i16 2)
  (cass-def-type 8 :i32 4)
  (cass-def-type 10 :i64 8) ;; a
  (cass-def-type 11 '(:string :utf7 :bytes :txt) nil) ;; b
  (cass-def-type 12 :struct nil) ;; c
  
  (cass-def-type 13 'cass-map nil) ;; d
  (cass-def-type 14 :set nil) ;; e
  (cass-def-type 15 'cass-list nil) ;; f

  (cass-def-type 16 :utf8 nil)
  (cass-def-type 17 :utf16 nil)

  (def ttype (sym) ;; TODO:macro-function
    (or
     (ht ttype-sym2id sym)
     (when (and (symbolp sym)
                (not (keywordp sym)))
       (ht ttype-sym2id :struct))
     (ht ttype-sym2id :bytes)))

  (def ttype-size-by-id (id)
    (ht ttype-id2size id))
  (def ttype-size (sym)
    (ttype-size-by-id (ttype sym)))
  (def ttype-id2sym (id)
    (ht ttype-id2sym id))

  (def tmtype (type)
    (case type
      (:call 1)
      (:reply 2)
      (:exception 3)
      (:onway 4)))

  ;; cassandra con lv
  (defvar cons-lv-sym2id (ht-gen))
  (defvar cons-lv-id2sym (ht-gen))
  (dolist (def-i (group 
                  '(
                    :one 1
                    :quorum 2 
                    :local_quorum 3
                    :each_quorum 4
                    :all 5
                    :any 6
                    :two 7
                    :three 8)
                  2))
    (ht cons-lv-sym2id (car def-i) (cadr def-i))
    (ht cons-lv-id2sym (cadr def-i) (car def-i)))

  (def cons-lv-sym2id (sym)
    (ht cons-lv-sym2id sym))
  (def cons-lv-id2sym (id)
    (ht cons-lv-id2sym id))
  ;; enum is i32
  )

;;;;;;;;;;;
;;  void batch_mutate(1:required map<binary, map<string, list<Mutation>>> mutation_map,
;;                    2:required ConsistencyLevel consistency_level=ConsistencyLevel.ONE)
;;       throws (1:InvalidRequestException ire, 2:UnavailableException ue, 3:TimedOutException te),

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; util
(eval-always
  (def map-specs (fn specs &rest args)
    (s:collect
        (s:mapping ((spec (s:scan specs)))
                   (apply fn (append args spec)))))

  (def gen-struct (name specs)
    `(defstruct ,name
       ,@(map-specs (lambda (fid name type &key req-p)
                      `(,name ,@(if req-p `(nil :type (not null)))))
                    specs)))

  (def gen-pack-slot (name obj fid fname ftype &key &allow-other-keys ) ;; NAME:new-struct name. OBJ:instance of new-struct.
    (let ((accessor (intern (mkstr name "-" fname))))
      `(awhen (,accessor ,obj)
         (pack-wrapper it ,ftype ,fid))))

  (def gen-pack (name specs)
    (let ((obj (gensym "obj-"))
          (fid (gensym "fid-")))
      `(defmethod pack ((,obj ,name)) ;; &optional ,fid)
         ;; fields
         ,@(map-specs #'gen-pack-slot specs name obj)
         ;; termination
         (pack-wrapper 0 :i8))
      ))

  (defmacro w/cass-os (&body body)
    `(progv '(cass-os) '(nil)
       ,@body))

  )

;;;; api
(defmacro cass-def-struct (name &body specs)
  `(progn
     ,(gen-struct name specs)
     ,(gen-pack name specs)))

;;;; compiler
(defgeneric pack (obj)) ;;  &optional fid))

(defun pack-wrapper (obj type &optional fid)
  (when fid
    (pack-wrapper (ttype type) :i8)
    (pack-wrapper fid :i16))

  (cond
    ((eq :bool type)
     (pack-wrapper obj :ui8))

    ((eq :txt type) ;; dirty. in original spec, :txt equals to :bytes.
     (multiple-value-bind (txt-oct len) (t2o obj)
       (pack-wrapper len :i32)
       (when (< 0 len)
         (push txt-oct cass-os))))

    ((eq :bytes type)
     (w1 (len (length obj))
       (pack-wrapper len :i32)
       (when (< 0 len)
         (push obj cass-os))))

    ;; ((eq :map type)
    ;;  )
    ;; ((eq :list type)
    ;;  )

    ((keywordp type)
     (push (val2o-dyn obj type) cass-os))

    (t ;; struct, map, list or somethng.
     (pack obj))))



(def pack-message (obj &key (mtype :call) (seqid 0))
  (logd ">>~s" mtype)
  (w/cass-os
    (let ((tgt ;;(let ((cass-os nil))
           (progn
             (pack-wrapper 0 :i32) ;; 0 is dummy size.
             (pack-wrapper (logior #x80010000 (tmtype mtype)) :i32)
             (pack-wrapper (2txt (type-of obj)) :txt)
             (pack-wrapper seqid :i32)

             (pack obj)

             (apply #'bytes-cat (nreverse cass-os))
             )))
      (let ((tgt-len (- (length tgt) 4)))
        (val2o-dyn tgt-len :i32 :oct tgt) ;; length
        ))))

;;;; map
(defstruct cass-map
  (ktype :txt)
  (vtype :txt)
  (map (ht-gen :test 'equal)))
(def cass-map-len (a)
  (hash-table-count (cass-map-map a)))

(defmethod pack ((obj cass-map)) ;; &optional fid)
  (pack-wrapper (ttype (cass-map-ktype obj)) :i8)
  (pack-wrapper (ttype (cass-map-vtype obj)) :i8)
  (pack-wrapper (cass-map-len obj) :i32)

  (s:iterate (((k v) (s:scan-hash (cass-map-map obj)))) ;; TODO:Must I handle structure like things?
    (pack-wrapper k (cass-map-ktype obj))
    (pack-wrapper v (cass-map-vtype obj)))
  )

;;;; list
(defstruct cass-list
  (vtype :txt)
  (list nil))
(def cass-list-len (a)
  (length (cass-list-list a)))
(defmethod pack ((obj cass-list))
  (pack-wrapper (ttype (cass-list-vtype obj)) :i8)
  (pack-wrapper (cass-list-len obj) :i32)

  (s:iterate ((v (s:scan (cass-list-list obj))))
      (pack-wrapper v (cass-list-vtype obj)))
  )

;;;; cassandra struct definition
(cass-def-struct column
  (1 name :bytes :req-p t)
  (2 value :bytes)
  (3 timestamp :i64)
  (4 ttl :i32))

(cass-def-struct super-column
  (1 name :bytes :req-p t)
  (2 value :i64 :req-p t))

(cass-def-struct counter-column
  (1 name :bytes :req-p t)
  (2 value :i64 :req-p t))

(cass-def-struct counter-super-column
  (1 name :bytes :req-p t)
  (2 columns 'cass-list :req-p t) ;; contain counter-column
  )

(cass-def-struct column-or-super-column
  (1 column 'column)
  (2 super-column 'super-column)
  (3 counter-column 'counter-column)
  (4 counter-super-column 'counter-super-column))

(cass-def-struct column-parent ;; CF
  (3 column-family :txt :req-p t)
  (4 super-column :bytes))

(cass-def-struct column-path ;; CF->Col
  (3 column-family :txt :req-p t)
  (4 super-column-name :bytes)
  (5 column-name :bytes)) ;; column-name

(cass-def-struct slice-range
  (1 start  :bytes  :req-p t)
  (2 finish :bytes  :req-p t)
  (3 reversed :bool :req-p t)
  (4 count :i32 :req-p t))

(cass-def-struct slice-predicate
  (1 column-names 'cass-list) ;; contains bytes
  (2 slice-range 'slice-range))

(cass-def-struct deletion
  (1 timestamp :i64)
  (2 super-column :bytes)
  (3 predicate 'slice-predicate))

(cass-def-struct mutation
  (1 column-or-supercolumn 'column-or-supercolumn)
  (2 deletion 'deletion))

(cass-def-struct slice-predicate
  (1 column-names 'cass-list) ;; contain bytes.
  (2 slice-range 'slice-range))

;;;; cassandra message
(cass-def-struct |set_keyspace|
  (1 keyspace :txt :req-p t))

;;;; get
(cass-def-struct |get|
  (1 key :bytes :req-p t)
  (2 column-path 'column-path :req-p t) ;; 
  (3 consistency-level :i32 :req-p t))

(cass-def-struct |get_slice| ;; good for index search. get by range.
  ;; RETURN: list<ColumOrSuperColumn>
  (1 key :bytes :req-p t)
  (2 column-parent 'column-parent :req-p t)
  (3 slice-predicate 'slice-predicate :req-p t)
  (4 consistency-level :i32 :req-p t))

(cass-def-struct |multiget_slice| ;; good for entities get. get mult data by multi-key which I got by get_slice.
  ;; RETURN: map<binary, list<ColumOrSuperColumn>>
  (1 keys 'cass-list :req-p t) ;; contains bytes
  (2 column-parent 'column-parent :req-p t)
  (3 slice-predicate 'slice-predicate :req-p t)
  (4 consistency-level :i32 :req-p t))

;;;; modify
(cass-def-struct |insert|
  (1 key :bytes :req-p t)
  (2 column-parent 'column-parent :req-p t)
  (3 column 'column :req-p t)
  (4 consistency-level :i32 :req-p t))

(cass-def-struct |batch_mutate|
  (1 mutation-map 'cass-map :req-p t) ;; map<binary, map<string, list<Mutation>>>
  (2 consistency-level :i32 :req-p t))

(cass-def-struct |remove|
  (1 key :bytes :req-p t)
  (2 column-path 'column-path :req-p t)
  (3 timestamp :i64 :req-p t)
  (4 consistency-level :i32 :req-p t))

(cass-def-struct |add| ;; good for counter incf.
  (1 key :bytes :req-p t)
  (2 column-parent 'column-parent :req-p t)
  (3 counter-column 'counter-column :req-p t)
  (4 consistency-level :i32 :req-p t))

;;;; cass system api
(cass-def-struct column-def
  (1 name :bytes :req-p t)
  (2 validation-calss :txt :req-p t)
  (3 index-type 'index-type)
  (4 index-name :txt)
  (5 index-options 'cass-map)) ;; map<string,string>

(cass-def-struct cf-def
  (1 keyspace :txt :req-p t)
  (2 name :txt :req-p t)
  (3 column-type :txt) ;; Standard, CounterColumnType
  (13 column-metadata 'cass-list) ;; contains column-def
  (15 default-validation-class :txt) ;; you set CounterColumnType when you wanna counter column.
  )

(cass-def-struct ks-def
  (1 name :txt :req-p t)
  (2 strategy-class :txt :req-p t)
  (3 strategy-options 'cass-map) ;; map<string,string>
  (6 durable-writes :bool))

(cass-def-struct |system_add_column_family|
  (1 cf-def 'cf-def :req-p t))

;; 


;;;;;; util to shorten code for making cass-structure
(def cass-mk-list (type &rest rest)
  (make-cass-list :vtype type :list rest))
(def cass-mk-map (ktype vtype &rest rest)
  (make-cass-map :ktype ktype :vtype vtype
                 :map (ht-gen :test (case ktype
                                      (:txt 'equal)
                                      (:bytes 'oct-equal)
                                      (t 'equal))
                              :hash-function 'sxhash
                              :v rest)))

;;(cass-mk-map :txt :txt "aa" "bb" "cc")
                 

(def cass-mk-column-path (column-family &optional column-name) ;;
  (make-column-path :column-family column-family
                    :column-name  (when column-family 
                                    (if (typep column-name 'string) 
                                        (t2o column-name)
                                        column-name))))


(def cass-mk-mutation-column ( column )
  (make-mutation :column-or-supercolumn (make-column-or-super-column :column column)))

(def cass-mk-mutation-column2 ( name value &optional timestamp ttl )
  (unless timestamp 
    (setq timestamp (n-time-usec)))
  (cass-mk-mutation-column (make-column :name name :value value :timestamp timestamp :ttl ttl)))

(def cass-mk-mutation-deletion-column-names ( timestamp type &rest column-names)
  (make-mutation :deletion (make-deletion :timestamp timestamp
                                          :predicate (make-slice-predicate
                                                      :column-names (make-cass-list :vtype type :list column-names))))
  )

(def cass-mk-slice-predicate-range ( start finish &key (reversed 0) (count 100))
  (m::make-slice-predicate :slice-range (m::make-slice-range :start start :finish finish 
                                                             :reversed reversed :count count)))

;; cass does not yet support.
;; (def cass-mk-mutation-deletion-slice ( timestamp start finish &key (reversed 0) (count 100) ) ;; TODO:I need count, reversed
;;   (make-mutation :deletion (make-deletion :timestamp timestamp
;;                                           :predicate (make-slice-predicate 
;;                                                       :slice-range (make-slice-range :start start :finish finish
;;                                                                                      :reversed reversed :count count))))
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; READ
(defvar cass-is-bytes nil "cassandra in-stream bytes")
(defvar cass-is-idx nil "cassandra in-stream index")

(defmacro! cass-read (type)
  "primitive reader. you must indicate type.
cass-is-bytes and cass-is-idx are inserted symbols.It's a little dirty."
  (case type
    (:bool
     `(let ((,g!tmp (cass-read :ui8)))
       (if (eql 0 ,g!tmp) nil t)))
    (:txt
     `(let ((,g!len (cass-read :i32)))
       (let ((,g!old-idx cass-is-idx))
         (incf cass-is-idx ,g!len)
         (o2t cass-is-bytes :start ,g!old-idx :end cass-is-idx))))
    (:bytes ;; dirty. In original spec, txt equals to bytes.
     `(let ((,g!len (cass-read :i32)))
       (let ((,g!old-idx cass-is-idx))
         (incf cass-is-idx ,g!len)
         (subseq cass-is-bytes ,g!old-idx cass-is-idx)))) ;; subseq is not need and expensive.
    (t 
     `(let ((,g!old-idx cass-is-idx))
       (incf cass-is-idx (ttype-size ,type))
       (o2val-dyn cass-is-bytes ,type :start ,g!old-idx)))))

(defmacro! w/r-msg ((sym-name sym-ver sym-seqid) &body body)
  "read cassandra response message. read header and do body"
  `(let* ((,sym-ver (cass-read :i32))
        (,sym-name (cass-read :txt))
        (,sym-seqid (cass-read :i32)))
     ,@body))

(def cass-read-auto (&key type-id fid &aux res)
  "read cassandra response message recursively.convert structure to hash-table"
  (unless type-id
    (setq type-id (cass-read :i8))
    (when (zerop type-id)
      (return-from cass-read-auto (vals nil 0 0)))
    (setq fid (cass-read :i16)))
  (cond
    ((eql type-id (ttype :struct)) ;; struct
     (setq res (ht-gen))
     (tagbody
      :loop
        (multiple-value-bind (obj type-id fid) (cass-read-auto)
          (when (zerop type-id)
            (go :end))
          (ht res fid obj)
          (go :loop))
      :end 
        nil))

    ((eql type-id (ttype 'cass-map)) ;; map
     (let ((k-type (cass-read :i8))
         (v-type (cass-read :i8))
         (map-len (cass-read :i32)))
       (setq res (ht-gen :test 'equal-bytes :hash-function 'sxhash))
       (dotimes (i map-len)
         (ht res
             (cass-read-auto :type-id k-type :fid 0)
             (cass-read-auto :type-id v-type :fid 0)))))

    ((eql type-id (ttype 'cass-list)) ;; list
     (let ((elem-type (cass-read :i8)) ;; dum
         (list-len (cass-read :i32)))
       (setq res nil)
       (dotimes (i list-len)
         (push (cass-read-auto) res)
         ;;(setf (aref res i) (cass-read-auto))
         (cass-read :i8)))
             
       ;;(setq res (make-array list-len))
       ;;(dotimes (i list-len)
       ;;  (setf (aref res i) (cass-read-auto))
       ;;  (cass-read :i8)))
     )

    ((eql type-id (ttype :bool))
     (let ((tmp (cass-read :ui8)))
       (setq res (if (eql 0 tmp) nil t))))

    ((eql type-id (ttype :bytes))
     (let ((len (cass-read :i32)))
      (let ((old-idx cass-is-idx))
         (incf cass-is-idx len)
         (setq res (subseq cass-is-bytes old-idx cass-is-idx))))) ;; subseq is too expensive.

    (t
     (let ((old-idx cass-is-idx)
         (ttype-sym (ttype-id2sym type-id)))
       (incf cass-is-idx (ttype-size ttype-sym))
       (setq res (o2val-dyn cass-is-bytes ttype-sym :start old-idx)))))

  (values res type-id fid))



;;;; dumper. mainly for debug use.
(def cass-dump-ret (ret)
  (typecase ret
    (hash-table
     (s:collect
         (s:mapping (((k v) (s:scan-hash ret)))
                    (list (format nil "~a:" k) (cass-dump-ret v)))))
    (list
     (s:collect
         (s:mapping ((v (s:scan ret)))
                    (cass-dump-ret v))))
    (bytes (o2t ret))
    (t ret)
    ))

(def cass-slice2map (slice &key
                           (key-conv (l(x) (o2t x)))
                           (obj (ht-gen :test 'equal)))
  (s:iterate ((a (s:scan slice)))
      (ht obj 
          (funcall key-conv (ht a 1))
          (ht a 2)))
  obj)

;;;; column-family read write
(defgeneric cf-write (obj &key timestamp ttl))
(defgeneric cf-read (obj list))

(eval-always
  (defun _val2o (obj type)
    (case type
      (:txt (t2o obj))
      (t (val2o-dyn obj type))))
  (defun _o2val (o type)
    (case type
      (:txt (o2t o))
      (t (o2val-dyn o type))))

  (def struct-name (spec)
    (intern (2txt "CF-" spec)))

  (def gen-cf-struct (spec fields) ;; to make it simple, spec is name string.
    `(defstruct ,(struct-name spec)
       ,@(mapcar
          (lambda(x)
            (destructuring-bind (fname ftype) x
              `(,fname)))
          fields)))

  (def gen-cf-writer (spec fields)
    (w/uniq (g!list)
      ;;(let ((g!obj (gensym "obj"))
      ;;      (g!lst (gensym "lst")))
      `(defmethod cf-write ((obj ,(struct-name spec)) &key timestamp ttl
                            &aux ,g!list)
         ,@(mapcar
            (lambda(x)
              (destructuring-bind (fname ftype) x
                ;;(lambda(fname ftype)
                (let ((accessor (intern (2txt (struct-name spec) "-" fname))))
                  `(awhen (,accessor obj)
                     (push (cass-mk-mutation-column2 ,(t2o (2txt fname)) (_val2o it ,ftype) timestamp ttl)
                           ,g!list)))))
            fields)
         (apply #'m::cass-mk-list
                'm::mutation ,g!list))))

  ;;(defmacro test (struct-name access-name obj val)
  ;;  `(setf (,(intern (2txt struct-name "-" access-name)) ,obj) ,val))

  (def gen-cf-reader (spec fields)
    (let ((g!obj (gensym "OBJ"))
          (g!list (gensym "LIST"))
          (g!i (gensym "I"))
          (g!i-nam (gensym "NAM"))
          (g!i-val (gensym "VAL"))
          (g!fields-ht (gensym "FIELD-HT"))
          (fields-ht (ht-gen :test 'equal)))
      (dolist (f fields)
        (ht fields-ht (2txt (car f)) (cadr f)))

      `(let ((,g!fields-ht ,fields-ht))
         (defmethod cf-read ((,g!obj ,(struct-name spec)) ,g!list)
           (dolist (,g!i ,g!list)
             (let ((,g!i-nam (o2t (ht ,g!i 1)))
                   (,g!i-val (ht ,g!i 2)))
               (awhen (ht ,g!fields-ht ,g!i-nam)
                 (funcall (fdefinition (list 'setf (intern (2txt ',(struct-name spec) "-" ,g!i-nam) ,(symbol-package spec))))
                     (_o2val ,g!i-val (ht ,g!fields-ht ,g!i-nam))
                     ,g!obj)

                 (logd ">>>>B")
                 )))))))

  (def gen-cf-definer (spec fields)
    `(def ,(intern (2txt "CF-DEFINER-OF-" spec)) (keyspace &rest rest)
       (m::make-cf-def :keyspace keyspace :name ,(2txt spec) :column-type "Standard")
       ))

  (def gen-cf-slice-predicate (spec fields)
    `(def ,(intern (2txt "CF-SLICE-PREDICATE-OF-" spec)) ()
       (make-slice-predicate :column-names 
                             (cass-mk-list :txt 
                               ,@(mapcar (lambda(x)
                                           (destructuring-bind (fname ftype) x
                                             (2txt fname)))
                                         fields)))))
            
  )

(defmacro def-cf (spec &body fields)
  `(progn
     ,(gen-cf-struct spec fields)
     ,(gen-cf-writer spec fields)
     ,(gen-cf-reader spec fields)
     ;;,(gen-cf-definer spec fields)
     ,(gen-cf-slice-predicate spec fields)
     
     (def ,(intern (2txt "CF-DEFINER-OF-" spec)) (keyspace &rest rest)
       (m::make-cf-def :keyspace keyspace :name ,(2txt spec) :column-type "Standard"))

     (def ,(intern (2txt "CF-COLUMN-PARENT-OF-" spec)) ()
       (m::make-column-parent :column-family ,(2txt spec)))
     ))



