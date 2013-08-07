;;; ToDo:


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
	(write-byte (car seq) stream)
	(write-byte-sequence (cdr seq) stream))
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
(defun make-mysin (frequency)
(let ((pf2 (* frequency 2 pi)))
    #'(lambda (x) 
	(nth-value 0 (round (* (sin (* x pf2)) 32767)))
	)
	)
) 
(defun mysin (x frequency)
	(nth-value 0 (round (* (sin (* x 440 2 pi)) 32767)))
)

(defun get-data-chunk ()
	(let ((xsin (make-mysin 440)))
	(setf start (get-universal-time))
	(setf listedata (concatenate 'list
	(strtobyte "data")
	(list 192 127 161 0)
	(loop for i
	from 1 
	to 10584000
	;;;collect (* (mysin i 440) 128))
	;;;collect (numbertoxbyte (random 550) 2))
	collect (numbertoxbyte (funcall xsin i) 2))
	))
	(setf ende (get-universal-time))
	listedata)
)


(defun testwrite ()
	(with-open-file (s "testwave.wav"  :direction :output :element-type 'unsigned-byte)
		(write-byte-sequence 
		(concatenate 'list
		(get-wave-header 0)
		(get-fmt-chunk)
		(get-data-chunk)) s)
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
