;; s1 = Tief
;; s2 = Hoch
(defun auftakt_s1 ()
	"'g8 'f#8"
)

(defun auftakt_s2 ()
	"e'8 f#'8"
)

(defun part1_s1 ()
	"'e4 'a4 'h4 'a4 'g4 'f#4 'e4 'f#4 'g4 'a4 'h4 'a4 'g4 'h4 'e8 'f#8 'g8 'f#8 'e4 'a4 'h4 'a4 'g4 'f#4 'e4 'f#4 'g4 c4 d4 'd4 'g2 'g4"
)

(defun part1_s2 ()
	"g'4 f#'8 e'8 d#'4 e'8 f#'8 h4 c#'8 d#'8 e'4 d'8 c'8 h4 a8 g8 f#4 g8 a8 h8 a8 g8 f#8 e4 e'8 f#'8 g'4 f#'8 e'8 d#'4 e'8 f#'8 h4 c#'8 d#'8 e'4 d'8 c'8 h4 a8 g8 f#4 f#8 g8 g2 g4"
)

(defun part2_s1 ()
	"'g4 'f#4 'd4 'g4 'h4 c4 'g#4 'a4 d4 e4 'a4 e4 'e4 'a8 'h8 'a8 'g8 'f#4 'd4 'g4 'h4 c4 'g#4 'a4 c#4 d4 'a#4 'h4 e4 f#4 'f#4 'h8 'a#8 'h8 c#8 d#4 'h4 e4 d4 c#4 'a4 d4 c4 'h4 'g4 c4 'h4 'a4 'f#4 'h4 'h8 c8 'h8 'a8 'g#4 'a8 e8 f#4 'g8 d#8 e4 'f#8 c#8 d#4 'e4 'a4 'h4 'a4 'h2 'e2 'e4" 
)

(defun part2_s2 ()
	"h8 g8 d'4 a8 c'8 h4 g'8 d'8 e'4 h8 d'8 c'4 h8 a8 g#4 a8 h8 c'4 h8 a8 a2 a4 d'8 a8 h4 g'8 d'8 e'4 h8 d'8 c'4 a'8 e'8 f#'4 c#'8 e'8 d'4 c#'8 h8 a#4 a#8 h8 h2 h4 h'8 f#'8 g#'4 f#'8 e'8 a'4 e'8 g'8 f#'4 e'8 d'8 g'4 d'8 f'8 e'4 a'8 e'8 f#'4 c#'8 e'8 d#'4 h2 e'8 h8 c'4 d'8 a8 h4 c'8 g8 a4 h8 f#8 g4 f#8 e8 d#4 e8 f#8 g4 f#8 e8 e2 e4"
)

(defun bourree ()
  (list (concatenate 'string "p2 p4 "(auftakt_s1) " " (part1_s1) " " (auftakt_s1) " " (part1_s1) " " (part2_s1) " " (part2_s1) "p1")
		(concatenate 'string "p2 p4 "(auftakt_s2) " " (part1_s2) " " (auftakt_s2) " " (part1_s2) " " (part2_s2) " " (part2_s2) "p1"))
)