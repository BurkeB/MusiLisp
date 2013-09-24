(defpackage "TONE"
  (:use "COMMON-LISP")
  (:export #'get-toneseconds 
		   #'get-frequency))

(in-package tone)

;;; Functions for converting a tone into a frequency. 
;;; For example:
;;; (get-frequency "a'") => 440
;;; (get-frequency "f") => ~174

(defun string-to-list (string)
	(if (null string)
		nil
		(if (= (length string) 1)
		(cons (subseq string 0 1) nil)
		(cons (subseq string 0 1) (string-to-list (subseq string 1)))
		)
	)
)

(defun calc-toneseconds (bpm length)
	(declare (integer bpm) (integer length))
	(float (* (* (/ 60 bpm) 4) (expt length -1)))
)

(defun extract-numbers-from-list (list)
	(if (null list)
		nil
		(if (numberp (parse-integer (car list) :junk-allowed t))
			(cons (car list) (extract-numbers-from-list (cdr list)))
			(extract-numbers-from-list (cdr list))
		)
	)
)

(defun get-tone (tone)
	(cond 
		((equal tone "c") 1)
		((equal tone "d") 3)
		((equal tone "e") 5)
		((equal tone "f") 6)
		((equal tone "g") 8)
		((equal tone "a") 10)
		((equal tone "h") 12)
		((equal tone "#") 1)
		((equal tone "b") -1)
		(T 0)
	)
)

(defun get-tone-value (tone &optional (value -12))
			(if (null tone)
				0
				(if (equal (car tone) "'")
					(+ value (get-tone-value (cdr tone) value))
					(+ (get-tone (car tone)) (get-tone-value (cdr tone) (abs value)))
				)
			)
)

(defun get-frequency (tone)
	(assert (stringp tone) (tone))
	(if (null tone)
		nil
		(let ((difference (- (get-tone-value (string-to-list tone)) (get-tone-value (string-to-list "a'")) )))
			(* 440 (expt 2 (/ difference 12)))
		)
	)
)

(defun get-volume (tone)
	(assert (stringp tone) (tone))
	(- (count #\+ tone) (count #\- tone))
)

(defun get-length (tone)
	(nth-value 0 (parse-integer (remove-if-not #'digit-char-p tone) :junk-allowed t))
)

(defun get-toneseconds (bpm tone)
	(calc-toneseconds bpm (get-length tone))
)
