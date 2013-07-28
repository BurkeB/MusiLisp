;;; ToDo:
;;; Integer to x bytes: 
;;; countxbits --- Startbit beachten!

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
	(write-byte (car seq) stream)
	(write-byte-sequence (cdr seq) stream))
)

(defun get-wave-header (filesize)
  (concatenate 'list
   (strtobyte "RIFF")
   (list 0 161 127 228)
   (strtobyte "WAVE"))
)

(defun get-fmt-chunk ()
	(concatenate 'list
	(strtobyte "fmt ")
	(list 0 0 0 16) ;;; Header Size
	(list 0 1) 		;;; Format Tag = PCM
	(list 0 1)		;;; Channels = Mono
	(list 0 0 172 68) ;;; Sample-Rate = 44100hz
	(list 0 1 88 136)		;;; Bytes per Second	
	(list 0 2)		;;; Bytes per Sample (channels * ((bits+7)/8))
	(list 0 16)		;;; Bits per Sample = 16
	)
)

(defun get-data-chunk ()
	(concatenate 'list
	(strtobyte "data")
	(list 0 161 127 192)
	(loop for i
	from 1 
	to 10584000
	collect (random 255))
	(loop for i
	from 1 
	to 10584000
	collect (random 255))
	)
)


(defun testwrite ()
	(with-open-file (s "testwave.wav"  :direction :output :element-type 'unsigned-byte)
		(write-byte-sequence 
		(concatenate 'list
		(get-wave-header 0)
		(get-fmt-chunk)
		(get-data-chunk)) s)
	)
)
