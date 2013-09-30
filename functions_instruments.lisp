(defun squarewave (x frequency maxvolume samples_per_second)
    (declare
    (integer frequency)
    (integer maxvolume)
    (integer samples_per_second))
	(if (equal frequency 0)
	0
    (let* ((t2 (/ samples_per_second frequency))
          (t1 (/ t2 2))
          (t3 (mod x t2))
            (value maxvolume))
        (if (< t3 t1)
            value
            (* -1 value)
        )
    )
	)
)


(defun make-squarewave (frequency maxvolume samples_per_second)
    #'(lambda (x)
        (squarewave x frequency maxvolume samples_per_second)
    )
)



(defun mysin (x 2pf maxvolume samples_per_second)
    (let* (    (x2 (/ (float (mod x samples_per_second)) (float samples_per_second)))
            (y (nth-value 0 (round (* (sin (* x2 2pf) ) maxvolume)))))
        y
    )
)

(defun make-harmonic-instrument (frequency maxvolume samples_per_second)
    (declare (integer maxvolume) (integer frequency))
    #'(lambda (x)
        (let (    (base (mysin x frequency maxvolume samples_per_second))
				
                (harm1    (squarewave x (* frequency 4) (*(/ maxvolume 100) 20) samples_per_second))
                (harm2    (mysin x (+ (* frequency 2)) (*(/ maxvolume 100) 30) samples_per_second))
				)
                (nth-value 0 (truncate (+ base harm1 harm2) 2))
        )
    )
)




(defun make-mysin (frequency maxvolume samples_per_second)
(declare (integer frequency))
(let ((pf2 (* frequency 2 pi)))
    #'(lambda (x)
    (declare (integer x))
        (let ((y (mysin x pf2 maxvolume samples_per_second)))
            y
        )
    )
    )
)

(defun make-mysin-octave (frequency maxvolume samples_per_second)
	(make-mysin (* frequency 2) maxvolume samples_per_second)
)

(defun make-harmonic-octave (frequency maxvolume samples_per_second)
	(make-harmonic-instrument (* frequency 2) maxvolume samples_per_second)
)