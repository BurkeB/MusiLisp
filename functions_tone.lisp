(in-package #:musilisp)

;;; Functions for converting a tone into a frequency or calculate the length of a tone.

(defun string-to-list (string)
	"Split a string into a list of 1-character-strings.
	Parameters
	1: String"
    (if (null string)
        nil
        (if (= (length string) 1)
        (cons (subseq string 0 1) nil)
        (cons (subseq string 0 1) (string-to-list (subseq string 1)))
        )
    )
)

(defun calc-toneseconds (bpm length)
	"Calculate how many seconds a tone last by a given Tempo (in BPM) and tone-length.
	Parameters
	1: Tempo in BPM
	2: Tone-Length 8 for 1/8-Notes, 4 for 1/4-Notes etc."
    (declare (integer bpm) (integer length))
    (float (* (* (/ 60 bpm) 4) (expt length -1)))
)

(defun extract-numbers-from-list (list)
	"Extract numbers from a List of strings.
	Parameters
	1: List of Strings"
    (if (null list)
        nil
        (if (numberp (parse-integer (car list) :junk-allowed t))
            (cons (car list) (extract-numbers-from-list (cdr list)))
            (extract-numbers-from-list (cdr list))
        )
    )
)

(defun get-tone (tone)
	"Returns a value for a given tone-element. For example, d has the value 3 in the octave, # will have the value +1.
	This program works with the german notation. The german h is the english/american b.
	Parameters
	1: String with one Character in it."
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
			"This function calculates the relative tone-value. 
			' before the tone-name will be octave-down, after the tone-name ' will be octave-up.
			Parameters
			1: tone - a list generated from a complete tone, use string-to-list
			Optional: value - the negative value of a complete octave, standard is -12 (halftones)."
            (if (null tone)
                0
                (if (equal (car tone) "'")
                    (+ value (get-tone-value (cdr tone) value))
                    (+ (get-tone (car tone)) (get-tone-value (cdr tone) (abs value)))
                )
            )
)

;;; Short test: d#' should be 16.
;;; d=3, #=+1, '=+12
;;; (get-tone-value (string-to-list "d#'"))

(defun get-frequency (tone)
	"This function calculates the absolute frequency.
	Parameters:
	1: tone - a single String which defines the tone. For example a' for the higher octave of a(220hz)"
    (assert (stringp tone) (tone))
    (if (null tone)
        nil
		(if (search "p" tone)
			0
			(let ((difference (- (get-tone-value (string-to-list tone)) (get-tone-value (string-to-list "a'")) )))
				(* 440 (expt 2 (/ difference 12)))
			)
		)
    )
)

;;; Short test: a' should be 440
;;; 			a'' should be 880
;;; (get-frequency "a'")
;;; (get-frequency "a''")

(defun get-volume (tone)
	"This function is not used at the moment. Later in this project notes will have dynamic volume..
	Dont use this function."
    (assert (stringp tone) (tone))
    (- (count #\+ tone) (count #\- tone))
)

(defun get-length (tone)
	"This function gets the tone-length out of a string.
	Parameter
	1: a String which represents the tone."
    (nth-value 0 (parse-integer (remove-if-not #'digit-char-p tone) :junk-allowed t))
)

;;; (get-length "c#'8")

(defun get-toneseconds (bpm tone)
	"Returns the length of a tone in seconds.
	Parameter
	1: Tempo in BPM
	2: a String which represents the tone."
    (calc-toneseconds bpm (get-length tone))
)
