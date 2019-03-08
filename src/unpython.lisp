;;;; unpython.lisp

(in-package #:unpython)

(defvar +highest-protocol+ 4)

(defvar *protocol* 0)
(defvar *meta-stack* '())
(defvar *stack* '())
(defvar *memo* (make-hash-table))

(defun reset ()
  (setf *protocol* 0)
  (setf *stack* '())
  (setf *meta-stack* '())
  (setf *protocol* 0)
  (setf *memo* (make-hash-table)))

(defun load-pickle (file)
  (reset)
  (with-open-file (pickle file :element-type '(unsigned-byte 8))
    (let (res)
      (loop for op-code = (read-byte pickle nil nil)
            while op-code
            do (setf res (perform-op op-code pickle)))
      (setf *memo* (make-hash-table))
      res)))

(defun bytes->long (bytes)
  (reduce #'(lambda (f s)
              (+ (* s 256) f))
          bytes))

(defun print-table-keys (table)
  (loop for k being the hash-keys in table
        do (print k)))

(defun pop-mark ()
  (let ((items *stack*))
    (setf *stack* (pop *meta-stack*))
    items))

(do-for +proto+ (stream)
  (let ((protocol (read-byte stream)))
    (unless (<= 0 protocol +highest-protocol+)
      (error "+proto+ placeholder"))
    (setf *protocol* protocol)))

(do-for +empty-list+ ()
  (push '() *stack*))

(do-for +empty-dict+ ()
  (push (make-hash-table :test 'equal) *stack*))

(do-for +setitem+ ()
  (let ((values (pop *stack*))
        (key (pop *stack*))
        (dict (first *stack*)))
    (setf (gethash key dict) values)))

(do-for +setitems+ ()
  ;; Have to do the pop first so we actually have a dict
  (let ((items (pop-mark))
        (dict (first *stack*)))
    (loop for (i j) on items by #'cddr
          do (setf (gethash j dict) i))))

(do-for +binput+ (stream)
  (let ((i (read-byte stream)))
    (when (< i 0)
      (error "+binput+ placeholder"))
    (setf (gethash i *memo*) (first *stack*))))

(do-for +long-binput+ (stream)
  (let ((i (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence i stream)
    (setf (gethash (bytes->long i) *memo*) (first *stack*))))

(do-for +binget+ (stream)
  (push (gethash (read-byte stream) *memo*) *stack*))

(do-for +long-binget+ (stream)
  (let ((i (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence i stream)
    (push (gethash (bytes->long i) *memo*) *stack*)))

(do-for +mark+ ()
  (push *stack* *meta-stack*)
  (setf *stack* '()))

(do-for +binunicode+ (stream)
  (let ((len (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence len stream)
    (let ((data (make-array (bytes->long len))))
      (read-sequence data stream)
      (push (map 'string 'code-char data) *stack*))))

(do-for +append+ ()
  (push (pop *stack*) (first *stack*)))

(do-for +appends+ ()
  (loop for i in (pop-mark)
        do (push i (first *stack*))))

(do-for +stop+ ()
  (pop *stack*))