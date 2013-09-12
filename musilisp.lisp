;;; ToDo:
;;; -Sinus-Generator
;;; -Tone Lists (frequency, Seconds etc.)
;;; -Tonal Variations in Generatorfunction
;;; -More generic Code (samples per second etc.)

;;; =========================
;;; Wave File Structure
;;; =========================

;;; RIFF Header
;;; 4B "RIFF"
;;; 4B Datasize - 8
;;; 4B "WAVE"

;;; =========================

;;; FMT Section
;;; 4B "FMT"
;;; 4B FMT-Header-Size (without first 8B => 16Bytes)
;;; 2B Format-Tag 01 (PCM)
;;; 2B Channels (1=Mono, 2=Stereo ...)
;;; 4B Sample-Rate (44100, 48000...)
;;; 4B Bytes/Second (Sample-Rate * Frame-Größe
;;; 2B Block Align (Frame-Größe = <Anzahl der Kanäle> * ((<Bits/Sample (eines Kanals)>+7) / 8)   (Division ohne Rest))
;;; 2B Bits/Sample 

;;; =========================

;;; Data Section
;;; 4B "DATA"
;;; 4B Datablock-Length
;;; <Block Align> First Value
;;; <Block Align> Second Value
;;; <Block Align> .....

(require "functions_tone.lisp")

(defun numbertoxbyte (number bytecount)
	(if (> bytecount 0)
		(let ((masked (logand number 255)))
			(cons masked (numbertoxbyte (ash number -8) (1- bytecount)))
		)
		nil
	)
)

(defun ltoi (charlist)
  (mapcar #'char-int charlist)
)
(defun strtolist (list)
	(concatenate 'list list)
)

(defun strtobyte(str)
	(ltoi (strtolist str))
)

(defun write-byte-sequence (seq stream)
 (unless (null seq)
	(if (listp (car seq))
	(write-byte-sequence (car seq) stream)
	(write-byte (car seq) stream))
	(write-byte-sequence (cdr seq) stream)
	)
)

(defun get-wave-header (filesize)
  (concatenate 'list
   (strtobyte "RIFF")
   ;;;(list 0 161 127 228)
	(list 228 127 161 0)
   (strtobyte "WAVE"))
)

(defun get-fmt-chunk ()
	(concatenate 'list
	(strtobyte "fmt ")
	(numbertoxbyte 16 4) ;;; Header Size
	(numbertoxbyte 1 2) ;;; Format Tag = PCM
	(numbertoxbyte 1 2) ;;; Channels = Mono
	(numbertoxbyte 44100 4) ;;; Sample-Rate = 44100hz
	(numbertoxbyte 88200 4)		;;; Bytes per Second	
	(numbertoxbyte 2 2) ;;; Bytes per Sample (channels * ((bits+7)/8))
	(numbertoxbyte 16 2)		;;; Bits per Sample = 16
	)
)

(defun squarewave (x frequency maxvolume samples_per_second)
	(declare 
	(integer frequency)
	(integer maxvolume)
	(integer samples_per_second))	
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

(defun signedint-to-number (num)
	(if (> num 32767)
		(* (logxor num 65535) -1)
		num
	)
)


(defun number-to-signedint (num)
	(if (< num 0)
		;;;(+ 32768 (logxor (logand num 32767) 32767))
		(- 65535 (lognot num))
		(logand num 32767)
	)
)


(defun make-harmonic-instrument (frequency maxvolume samples_per_second)
	(declare (integer maxvolume) (integer frequency))
	#'(lambda (x)
		(let (	(base (mysin x frequency maxvolume samples_per_second))
				(harm1	(mysin x (* 2 pi (* frequency 8)) (*(/ maxvolume 100) 45) samples_per_second))
				(harm2	(mysin x (* 2 pi (+ (* frequency 10)(/ frequency 2))) (*(/ maxvolume 100) 45) samples_per_second)))
				;;;(harm1	(signedint-to-number (squarewave x (* frequency 8) (*(/ maxvolume 100) 45) samples_per_second)))
				;;;(harm2	(signedint-to-number (squarewave x (+ (* frequency 8)(/ frequency 1)) (*(/ maxvolume 100) 20) samples_per_second)))
				;;;(harm3	(signedint-to-number (squarewave x (+ (* frequency 10)(/ frequency 2)) (*(/ maxvolume 100) 25) samples_per_second)))
				;;;(harm4	(signedint-to-number (squarewave x (+ (* frequency 14)(/ frequency 1)) (*(/ maxvolume 100) 20) samples_per_second)))
				;;;(harm5	(signedint-to-number (squarewave x (+ (* frequency 18)(/ frequency 2)) (*(/ maxvolume 100) 25) samples_per_second))))
				(nth-value 0 (truncate (+ base harm1 harm2) 3))
		)
	)
)

(defun make-squarewave (frequency maxvolume samples_per_second)
	#'(lambda (x)
		(squarewave x frequency maxvolume samples_per_second)
	)
)



(defun mysin (x 2pf maxvolume samples_per_second)
	(let* (	(x2 (/ (float (mod x samples_per_second)) (float samples_per_second)))
			(y (nth-value 0 (round (* (sin (* x2 2pf) ) maxvolume)))))
		y
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


(defun write-tone (frequency seconds)
	(declare (integer frequency) (float seconds))
	(let* ((samples_per_second 44100)
		  (samples (* samples_per_second seconds))
			;;;(mysquarewave (make-harmonic-instrument frequency 32767 samples_per_second)))
			;;;(mysquarewave (make-squarewave frequency 32767 samples_per_second)))
			(mysquarewave (make-mysin frequency 32767 samples_per_second)))
		(loop for i
		from 0 
		to samples
		collect (numbertoxbyte (number-to-signedint (funcall mysquarewave i)) 2))
	)
)

(defun write-tone-list (liste)
	(unless (null liste)
		(cons (write-tone (tone:get-frequency (car liste)) 0.5) (write-tone-list (cdr liste)))
	)
)


(defun get-data-chunk (datacount)
	(concatenate 'list
	(strtobyte "data")
	(numbertoxbyte datacount 4)
	;;;(write-tone-list (list 440 660 880 660 440 660 880 1320))
	)
)

(defun testwrite ()
	(with-open-file (s "testwave.wav"  :direction :output :element-type 'unsigned-byte)
		(write-byte-sequence 
		(concatenate 'list
		(get-wave-header 0)
		(get-fmt-chunk)
		(get-data-chunk)
		) s)
	)
	(- ende start)
)

(defun testwrite2 ()
	(with-open-file (s "testwave.wav"  :direction :output :element-type 'unsigned-byte)
		(write-byte-sequence 
		(concatenate 'list
		(get-wave-header 0)
		(get-fmt-chunk)
		(get-data-chunk)) s)
	)
)

(defun write-melody (filename tonelist)
	(with-open-file (s filename  :direction :output :element-type 'unsigned-byte)
		(write-byte-sequence 
		(concatenate 'list
		(get-wave-header 0)
		(get-fmt-chunk)
		(get-data-chunk 10584000)
		(write-tone-list tonelist)
		) s)
	)
)

;;; (write-melody "../../tmp/testwave8.wav" '("c'" "d'" "e'" "f'" "g'" "g'" "a'" "a'" "a'" "a'" "g'" "a'" "a'" "a'" "a'" "g'"))