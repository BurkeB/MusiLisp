(in-package #:musilisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The function "split-by-one-space" comes from http://cl-cookbook.sourceforge.net/strings.html
;;;; The function "flatten" comes from http://rosettacode.org/wiki/Flatten_a_list#Common_Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-int (num)
	(nth-value 0 (round num)) 
)

(defun signedint-to-number (num)
    (if (> num 32767)
        (* (logxor num 65535) -1)
        num
    )
)


(defun number-to-signedint (num)
    (if (< num 0)
        (- 65535 (lognot num))
        (logand num 32767)
    )
)

(defun numbertoxbyte (number bytecount)
	"Convert a number to a list (little-Endian) of specific bytecount.
	Parameter
	1: number to convert
	2: number of bytes to represent the number"
    (if (> bytecount 0)
        (let ((masked (logand number 255)))
            (cons masked (numbertoxbyte (ash number -8) (1- bytecount)))
        )
        nil
    )
)

;;; (numbertoxbyte 439041101 4)
;;; (77 60 43 26)


(defun merge-lists (l1 l2)
	"Merges to lists with the same length.
	Parameters
	1: list1
	2: list2" 
    (map-into l1 #'(lambda (x y) (truncate (+ x y) 2)) l1 l2)
)

(defun ltoi (charlist)
	"Convert each char in the charlist to its bytevalue.
	Parameters
	1: List of chars"
  (mapcar #'char-int charlist)
)


(defun strtolist (str)
	"Convert String to a list of characters.
	Parameters
	1: string"
    (concatenate 'list str)
)

(defun strtobyte(str)
	"Converts a string to a list of bytes.
	Parameters
	1: string"
    (ltoi (strtolist str))
)

(defun convert-list (l1)
	"Converts the sample-values of a list to a wave-file-format.
	Parameters
	1: list filled with sample-values"
	(mapcar (lambda (n) (numbertoxbyte n 2)) l1))



(defun write-byte-sequence (seq stream)
	"Write a list of bytes into a writable bytestream.
	Parameters
	1: a list of bytes, or a list of lists of bytes...
	2: a stream which is accesable via write-byte"
	(unless (null seq)
	    (if (listp (car seq))
			(write-byte-sequence (car seq) stream)
			(write-byte (car seq) stream))
	    (write-byte-sequence (cdr seq) stream)
    )
)

(defun get-wave-header (filesize)
	"This returns a list of bytes which represent the Wave-file-header.
	Parameter
	1: filesize"
  (concatenate 'list
	(strtobyte "RIFF")
	(numbertoxbyte filesize 4)
	(strtobyte "WAVE"))
)

(defun get-fmt-chunk ()
	"This returns a list of bytes which represent the FMT-Chunk."
    (concatenate 'list
    (strtobyte "fmt ")
    (numbertoxbyte 16 4) ;;; Header Size
    (numbertoxbyte 1 2) ;;; Format Tag = PCM
    (numbertoxbyte 1 2) ;;; Channels = Mono
    (numbertoxbyte 44100 4) ;;; Sample-Rate = 44100hz
    (numbertoxbyte 88200 4)        ;;; Bytes per Second
    (numbertoxbyte 2 2) ;;; Bytes per Sample (channels * ((bits+7)/8))
    (numbertoxbyte 16 2)        ;;; Bits per Sample = 16
    )
)


(defun write-tone (frequency seconds instrument)
    (declare (integer frequency) (float seconds))
    (let* ((samples_per_second 44100)
			(factor (/ 1 samples_per_second))
            (samples (* samples_per_second seconds))
            (my-instrument (funcall instrument frequency)))
        (loop 
			for i from 0
			to samples
			collect (to-int (* (funcall my-instrument (* i factor)) 32767)))
    )
)

;;; (write-tone 120 1.0 #'mysin)

(defun write-tone-list (liste bpm instrument)
    (unless (null liste)
        (cons (write-tone (to-int (get-frequency (car liste))) (get-toneseconds bpm (car liste)) instrument) (write-tone-list (cdr liste) bpm instrument))
    )
)


(defun get-data-chunk (datacount)
    (concatenate 'list
    (strtobyte "data")
    (numbertoxbyte datacount 4)
    )
)



(defun get-samples (melody instrument bpm)
	(if (listp melody)
			(let* ((l1 (flatten (write-tone-list (split-by-one-space (car melody)) bpm instrument)))
				(l2 (flatten (write-tone-list (split-by-one-space (cadr melody)) bpm instrument))))
					(merge-lists l1 l2)
					(convert-list l1)
					l1
			)
			(let ((l1 (flatten (write-tone-list (split-by-one-space melody) bpm instrument))))
			(convert-list l1)
			l1
			)
	)
)

(defun musilisp (filename melody &key (instrument #'mysin) (bpm 120))
    (with-open-file (s filename  :direction :output :element-type 'unsigned-byte :if-exists :RENAME)
		(let* (
			  (all nil)
			  (samples (flatten (get-samples melody instrument bpm)))
			  (wave-header nil)
			  (fmt-chunk (get-fmt-chunk))
			  (data-chunk (get-data-chunk (length samples))))
			(setf all (concatenate 'list fmt-chunk data-chunk samples))
			(setf wave-header (get-wave-header (length all)))
			(setf all (concatenate 'list wave-header all))
			(write-byte-sequence all s)	
		)
    )
)

;;(musilisp "bourree_complete_sine_oct.wav" (bourree) :bpm 155 :instrument #'make-mysin-octave)
;;;; (musilisp "auftakt2.wav" 	(auftakt_s1) :bpm 155 :instrument #'mysin-octave)
;;;  (musilisp "bourree3.wav" (bourree) :bpm 135 :instrument #'mysin-octave)
;;;; (musilisp "bourree_complete_harmonic_oct.wav" (bourree) :bpm 155 :instrument #'make-harmonic-octave)
;;;; (musilisp "bourree_complete_squarewave_oct.wav" (bourree) :bpm 155 :instrument #'make-squarewave-octave)
