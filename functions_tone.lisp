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
					(+ (get-tone (car tone)) (get-tone-value (cdr tone) 12))
				)
			)
)

(defun get-frequency (tone)
	(if (null tone)
		nil
		(let ((difference (- (get-tone-value (string-to-list tone)) (get-tone-value (string-to-list "a'")) )))
			(* 440 (expt 2 (/ difference 12)))
		)
	)
)