;;;; clappy-space-program.asd

(asdf:defsystem #:clappy-space-program
  :description "Describe clappy-space-program here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:alexandria)
  :components ((:file "package")
               (:file "utils")
               (:file "game")
               (:file "gravity")
               (:file "clappy-space-program")))
