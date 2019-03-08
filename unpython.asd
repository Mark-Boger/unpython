;;;; unpython.asd

(asdf:defsystem #:unpython
  :description "Read pickle files into common lisp"
  :author "Mark Boger <93mar.bog@gmail.com>"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "opcodes")
               (:file "unpython")))
