;;; ToDo:
;;; Integer to x bytes: 
;;; countxbits --- Startbit beachten!


(loop for i
	downfrom (- 8 1)
	to 0
	append
	(list i))



(defun countxbits (startbit bitcount value)
	(let ((tmpval value)
		  (collector 0))
	(loop for i
			downfrom (- bitcount 1)
			to 0
			when (>= (/ tmpval (expt 2 i)) 1) 
			do (setf collector (+ collector (expt 2 i)))
			and 
			do (setf tmpval (- tmpval (expt 2 i))))
	collector
	)
)

;;; (> (/ 1 (expt 2 8)) 1)

(> 5 3)
(defun ltoi (charlist)
  (mapcar #'char-int charlist)
)

(defun strtolist (list)
	(concatenate 'list list)
)

(defun get-wave-header (filesize)
  (concatenate 'list
   (ltoi (strtolist "RIFF"))
   (list filesize)
   (ltoi (strtolist "WAVE")))
)