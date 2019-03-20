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
  (clrhash *memo*))

(defun load-pickle (file)
  (reset)
  (with-open-file (pickle file :element-type '(unsigned-byte 8))
    (handler-case
        (loop for op-code = (read-byte pickle nil nil)
              while op-code
              do (perform-op op-code pickle))
      (stop (stop-value)
        (clrhash *memo*)
        (return-from load-pickle (return-val stop-value)))))
  (error 'unpickling-error :code 'None
                           :error "Reached end of file before reading +STOP+ op code"))

(defun bytes->long (bytes)
  (reduce #'(lambda (f s)
              (+ (* s 256) f))
          bytes
          :from-end t))

(defun byte-sequence->utf-8-codes (sequence)
  (let ((len (1- (length sequence))))
    (loop for i to len
          collect (let ((byte (aref sequence i)))
                    (cond
                      ((<= byte #x7f)
                       byte)

                      ((<= #xc0 byte #xdf)
                       (let ((bs (list (logand byte #x3f)
                                       (logand (aref sequence (incf i))
                                               #x7f))))
                         (reduce #'(lambda (i b)
                                     (logior (ash i 6) b))
                                 bs)))

                      ((<= #xe0 byte #xef)
                       (let ((bs (list (logand byte #x0f)
                                       (logand (aref sequence (incf i)) #x7f)
                                       (logand (aref sequence (incf i)) #x7f))))
                         (reduce #'(lambda (i b)
                                     (logior (ash i 6) b))
                                 bs)))

                      ((<= #xf0 byte #xf7)
                       (let ((bs (list (logand byte #x07)
                                       (logand (aref sequence (incf i)) #x7f)
                                       (logand (aref sequence (incf i)) #x7f)
                                       (logand (aref sequence (incf i)) #x7f))))
                         (reduce #'(lambda (i b)
                                     (logior (ash i 6) b))
                                 bs)))

                      (t
                       (error (format nil "Invalide UTF-8: ~a~%" sequence))))))))

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
      (error 'unpickling-error :code '+proto+
                               :error (format nil
                                              "Protocol ~A not supported"
                                              protocol)))
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
      (error 'unpickling-error :code '+binput+
                               :error (format nil "Read byte less than 0 at pos ~A"
                                              (- (file-position stream) 1))))
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
      (push (map 'string 'code-char (byte-sequence->utf-8-codes data))
            *stack*))))

(do-for +append+ ()
  (push (pop *stack*) (first *stack*)))

(do-for +appends+ ()
  (loop for i in (pop-mark)
        do (push i (first *stack*))))

(do-for +stop+ ()
  (signal 'stop :return-val (pop *stack*)))
