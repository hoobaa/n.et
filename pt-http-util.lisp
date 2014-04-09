(in-package 'n.et)

#| util. (enc, dec, etc.) |#
(def** url-encode (string &optional (enc :utf-8))
  "from drakma"
  (when (eql enc :utf8)
    (setf enc :utf-8))
  (with-output-to-string (out)
    (loop for octet across (t2o (or string "") :enc enc)
       for char = (code-char octet)
       do (cond ((or (char<= #\0 char #\9)
                     (char<= #\a char #\z)
                     (char<= #\A char #\Z)
                     (find char "$-_.!*'()," :test #'char=))
                 (write-char char out))
                ((char= char #\Space)
                 (write-char #\+ out))
                (t (format out "%~2,'0x" (char-code char)))))))

(def** url-decode (string &optional (enc :utf-8))
  "from hanchentoot"
  (when (eql enc :utf8)
    (setf enc :utf-8))
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type 'octet :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
       (unless (< i (length string))
         (return))
       (let ((char (aref string i)))
         (labels ((decode-hex (length)
                    (prog1
                        (parse-integer string :start i :end (+ i length) :radix 16)
                      (incf i length)))
                  (push-integer (integer)
                    (vector-push integer vector))
                  (peek ()
                    (aref string i))
                  (advance ()
                    (setq char (peek))
                    (incf i)))
           (cond
             ((char= #\% char)
              (advance)
              (cond
                ((char= #\u (peek))
                 (unless unicodep
                   (setq unicodep t)
                   (upgrade-vector vector '(integer 0 65535)))
                 (advance)
                 (push-integer (decode-hex 4)))
                (t
                 (push-integer (decode-hex 2)))))
             (t
              (push-integer (char-code (case char
                                         ((#\+) #\Space)
                                         (otherwise char))))
              (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (o2t vector :enc enc)))))

(def** html-esc (string)
  (w/2txt (os)
    (loop for c across string
       do 
         (case c
           (#\< (write-string "&lt;" os))
           (#\> (write-string "&gt;" os))
           (#\" (write-string "&quot;" os))
           (#\' (write-string "&#039;" os))
           (#\& (write-string "&amp;" os))
           (otherwise (write-char c os))))))

(def** html-esc-2str (string os)
  (loop for c across string
     do 
       (case c
         (#\< (write-string "&lt;" os))
         (#\> (write-string "&gt;" os))
         (#\" (write-string "&quot;" os))
         (#\' (write-string "&#039;" os))
         (#\& (write-string "&amp;" os))
         (otherwise (write-char c os)))))

(def** html-quote (string)
  "Quotes string according to RFC 2616's definition of `quoted-string'."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
         while char
         unless (or (char< char #\Space)
                    (char= char #\Rubout))
         do (case char
              ((#\\) (write-string "\\\\" out))
              ((#\") (write-string "\\\"" out))
              (otherwise (write-char char out)))))))


(defconstant +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The three-character names of the seven days of the week - needed
for cookie date format.")

(defconstant +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The three-character names of the twelve months - needed for cookie
date format.")

(def* rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123.  Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))
