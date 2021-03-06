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

(defun bytes->long (bytes &key from-end (size 8))
  (flet ((big (int byte)
           (logior (ash int size) byte))
         (little (byte int)
           (logior (ash int size) byte)))
    (let ((f (if (null from-end)
                 #'big
                 #'little)))
      (reduce f bytes :from-end from-end))))

(defun byte-sequence->utf8-codes (sequence)
  (let ((len (1- (length sequence))))
    (macrolet ((gen (byte mask num)
                 (let ((byte-array (gensym)))
                   `(let ((,byte-array (list
                                        (logand ,byte ,mask)
                                        ,@(loop repeat num
                                                collect `(logand
                                                          (aref sequence (incf i))
                                                          #x7f)))))
                      (bytes->long ,byte-array :size 6)))))
      (loop for i to len
            collect (let ((byte (aref sequence i)))
                      (cond
                        ((<= byte #x7f)
                         byte)
                        ((<= #xc0 byte #xdf)
                         (gen byte #x3f 1))
                        ((<= #xe0 byte #xef)
                         (gen byte #x0f 2))
                        ((<= #xf0 byte #xf7)
                         (gen byte #x07 3))
                        (t
                         (error (format nil "Invalide UTF-8: ~a~%" sequence)))))))))

(defun pop-mark ()
  (let ((items *stack*))
    (setf *stack* (pop *meta-stack*))
    items))

(on-load +proto+ (stream)
  (let ((protocol (read-byte stream)))
    (unless (<= 0 protocol +highest-protocol+)
      (error 'unpickling-error :code '+proto+
                               :error (format nil
                                              "Protocol ~A not supported"
                                              protocol)))
    (setf *protocol* protocol)))

(on-load +empty-list+ ()
  (push '() *stack*))

(on-load +empty-dict+ ()
  (push (make-hash-table :test 'equal) *stack*))

(on-load +setitem+ ()
  (let ((values (pop *stack*))
        (key (pop *stack*))
        (dict (first *stack*)))
    (setf (gethash key dict) values)))

(on-load +setitems+ ()
  ;; Have to do the pop first so we actually have a dict
  (let ((items (pop-mark))
        (dict (first *stack*)))
    (loop for (value key) on items by #'cddr
          do (setf (gethash key dict) value))))

(on-load +binput+ (stream)
  (let ((i (read-byte stream)))
    (when (< i 0)
      (error 'unpickling-error :code '+binput+
                               :error (format nil "Read byte less than 0 at pos ~A"
                                              (- (file-position stream) 1))))
    (setf (gethash i *memo*) (first *stack*))))

(on-load +long-binput+ (stream)
  (let ((i (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence i stream)
    (setf (gethash (bytes->long i :from-end t) *memo*) (first *stack*))))

(on-load +binget+ (stream)
  (push (gethash (read-byte stream) *memo*) *stack*))

(on-load +long-binget+ (stream)
  (let ((i (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence i stream)
    (push (gethash (bytes->long i :from-end t) *memo*) *stack*)))

(on-load +mark+ ()
  (push *stack* *meta-stack*)
  (setf *stack* '()))

(on-load +binunicode+ (stream)
  (let ((len (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence len stream)
    (let ((data (make-array (bytes->long len :from-end t))))
      (read-sequence data stream)
      (push (map 'string 'code-char (byte-sequence->utf8-codes data))
            *stack*))))

(on-load +append+ ()
  (push (pop *stack*) (first *stack*)))

(on-load +appends+ ()
  (loop for i in (pop-mark)
        do (push i (first *stack*))))

(on-load +stop+ ()
  (signal 'stop :return-val (pop *stack*)))

