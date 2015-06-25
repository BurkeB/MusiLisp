;;;; musiLisp.asd

(asdf:defsystem #:musilisp
  :description "Describe MusiLisp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
	       (:file "functions_tone")
	       (:file "functions_instruments")
	       (:file "bourree")
               (:file "musilisp")))

