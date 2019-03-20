;;;; unpython.asd

(asdf:defsystem #:unpython
  :description "Read pickle files into common lisp"
  :author "Mark Boger <93mar.bog@gmail.com>"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "opcodes")
               (:file "conditions")
               (:file "unpython"))
  :in-order-to ((asdf:test-op (asdf:test-op :unpython-test))))

(asdf:defsystem #:unpython/test
  :description "Tests for unpytho"
  :author "Mark Boger <93mar.bog@gmail.com"
  :version "0.0.1"
  :depends-on (#:parachute)
  :serial t
  :pathname "t"
  :components ((:file "package")
               (:file "unpython"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :unpython-test)))
