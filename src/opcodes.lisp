;;;; Defines the different opcodes that the pickle protocol recognizes
(in-package #:unpython)

(defgeneric perform-op (op-code stream)
  (:documentation "Reads a pickle op-code and performs some operation")
  (:method (op-code stream)
    (declare (ignore stream))
    (error 'unpickling-error :code op-code :error "Not implemented")))

(defmacro on-load (op (&optional (stream nil)) &body body)
  (let ((op-code (gensym))
        (s (or stream (gensym))))
    `(defmethod perform-op ((,op-code (eql ,op)) ,s)
       ,(unless stream
          `(declare (ignore ,s)))
       ,@body)))

(macrolet ((defop (name code)
             `(defvar ,name ,(if (typep code 'character)
                                 `,(char-code code)
                                 `,code)))
           (defdoc (&rest args)
             `(progn
                ,@(loop :for (name docs) :on args :by #'cddr
                        :collect `(setf (documentation ',name 'variable) ,docs)))))
  (defop +mark+            #\()
  (defop +stop+            #\.)
  (defop +pop+             #\0)
  (defop +pop-mark+        #\1)
  (defop +dup+             #\2)
  (defop +float+           #\F)
  (defop +int+             #\I)
  (defop +binint+          #\J)
  (defop +binint1+         #\K)
  (defop +long+            #\L)
  (defop +binint2+         #\M)
  (defop +none+            #\N)
  (defop +persid+          #\P)
  (defop +binpersid+       #\Q)
  (defop +reduce+          #\R)
  (defop +string+          #\S)
  (defop +binstring+       #\T)
  (defop +short-binstring+ #\U)
  (defop +unicode+         #\V)
  (defop +binunicode+      #\X)
  (defop +append+          #\a)
  (defop +build+           #\b)
  (defop +global+          #\c)
  (defop +dict+            #\d)
  (defop +empty-dict+      #\})
  (defop +appends+         #\e)
  (defop +get+             #\g)
  (defop +binget+          #\h)
  (defop +inst+            #\i)
  (defop +long-binget+     #\j)
  (defop +list+            #\l)
  (defop +empty-list+      #\])
  (defop +obj+             #\o)
  (defop +put+             #\p)
  (defop +binput+          #\q)
  (defop +long-binput+     #\r)
  (defop +setitem+         #\s)
  (defop +tuple+           #\t)
  (defop +empty-tuple+     #\))
  (defop +setitems+        #\u)
  (defop +binfloat+        #\G)
  (defop +true+            (+ #.(char-code #\I) 0 #.(char-code #\newline)))
  (defop +false+           (+ #.(char-code #\I) 1 #.(char-code #\newline)))
  ;;;; Protocol 2
  (defop +proto+    #x80)
  (defop +newobj+   #x81)
  (defop +ext1+     #x82)
  (defop +ext2+     #x83)
  (defop +ext4+     #x84)
  (defop +tuple1+   #x85)
  (defop +tuple2+   #x86)
  (defop +tuple3+   #x87)
  (defop +newtrue+  #x88)
  (defop +newfalse+ #x89)
  (defop +long1+    #x8a)
  (defop +long4+    #x8b)
  ;;;; Protocol 3
  (defop +binbytes+       #\B)
  (defop +short-binbytes+ #\C)
  ;;;; Protocol 4
  (defop +short-binunicode+ #x8c)
  (defop +binunicode8+      #x8d)
  (defop +binbytes8+        #x8e)
  (defop +empty-set+        #x8f)
  (defop +additems+         #x90)
  (defop +frozenset+        #x91)
  (defop +newobj-ex+        #x92)
  (defop +stack-global+     #x93)
  (defop +memoize+          #x94)
  (defop +frame+            #x95)
  ;;;; Protocol 5
  (defop +bytearray8+      #x96)
  (defop +next-buffer+     #x97)
  (defop +readonly-buffer+ #x98)
  ;;;; Documentation
  (defdoc
      +mark+            "Push special markobject on stack"
    +stop+              "Marks the end of a pickle"
    +pop+               "Discard the topmost stack item"
    +pop-mark+          "Discard from the top of the stack to the topmost markobject"
    +dup+               "Duplicate the top stack item"
    +float+             "Push float object; decimal string argument"
    +int+               "Push integer or bool; decimal string argument"
    +binint+            "Push 4-byte signed int"
    +binint1+           "Push 1-byte unsigned int"
    +long+              "Push long; decimal string argument"
    +binint2+           "Push 2-byte unsigned int"
    +none+              "Push None"
    +persid+            "Push persistent object; id is taken from string arg"
    +binpersid+         "Push persistent object; id is taken from stack"
    +reduce+            "Apply callable to argtuple, both on stack"
    +string+            "Push string; newline terminated string argument"
    +binstring+         "Push string; counted binary string argument"
    +short-binstring+   "Push string; counted binary string argument < 256 bytes"
    +unicode+           "Push unicode string; raw unicode escaped argument"
    +binunicode+        "Push unicode string; counted UTF-8 string argument"
    +append+            "Append stack top to list below it"
    +build+             "Call __setstate__ or __dict__.update()"
    +global+            "Push self.find_class(modname, name); 2 string args"
    +dict+              "Build a dict from stack items"
    +empty-dict+        "Push empty dict"
    +appends+           "Extend list on stack by topmost stack slice"
    +get+               "Push item from memo on stack; index is string arg"
    +binget+            "Push item from memo on stack; index is 1-byte arg"
    +long-binget+       "Push item from memo on stack; index is 4-byte arg"
    +setitem+           "Add key+value pair to dict"
    +tuple+             "Build tuple from topmost stack items"
    +empty-tuple+       "Push empty tuple"
    +setitems+          "Modify dict by adding topmost key+value pairs"
    +binfloat+          "Push float; arg is 8-byte float encoding"
    +proto+             "Identify pickle protocol"
    +newobj+            "Build object by applying cls.__new__ to argtuple"
    +ext1+              "Push object from extension registry; 1-byte index"
    +ext2+              "Push object from extension registry; 2-byte index"
    +ext4+              "Push object from extension registry; 4-byte index"
    +tuple1+            "Build 1-tuple from stack top"
    +tuple2+            "Build 2-tuple from two topmost stack items"
    +tuple3+            "Build 3-tuple from three topmost stock times"
    +newtrue+           "Push True"
    +newfalse+          "Push False"
    +long1+             "Push long from < 256 bytes"
    +long4+             "Push really big long"
    +binbytes+          "Push bytes; counted binary string argument"
    +short-binbytes+    "Push bytes; counted binary string argument < 256 bytes"
    +short-binunicode+  "Push short string; UTF-8 length < 256 bytes"
    +binunicode8+       "Push very long string"
    +binbytes8+         "Push very long bytes string"
    +empty-set+         "Push empty set on the stack"
    +additems+          "Modify set by adding topmost stack items"
    +frozenset+         "Build frozenset from topmost stack items"
    +newobj-ex+         "Like +newobj+ but work with keyword only arguments"
    +stack-global+      "Same as +global+ but using names on the stacks"
    +memoize+           "Store the top of the stack in *memo*"
    +frame+             "Indicate the beginning of a new frame"
    +bytearray8+        "Push bytearray"
    +next-buffer+       "Push next out-of-band buffer"
    +readonly-buffer+   "Make the top of the stack readonly"))
