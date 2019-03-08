;;;; Defines the conditions specific to unpickling an object

(in-package #:unpython)

(define-condition unpickling-error (error)
  ((error-string :accessor error-string
                 :initarg :error)
   (op-code :accessor op-code
            :initarg :code))
  (:report (lambda (condition stream)
             (format stream "Error during unpickling ~a: ~a~%"
                     (op-code condition)
                     (error-string condition)))))

(define-condition stop ()
  ((value :accessor return-val
          :initarg :return-val)))
