(in-package #:musilisp)

(defun mysin (frequency)
	(let ((factor (* 2 frequency pi)))
		#'(lambda (x)
			(sin (* factor x))
	    )
	)
)

(defun mysin-octave (frequency)
	(mysin (* 2 frequency))
)
