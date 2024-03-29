(defpackage :um
  (:use :cl)
  (:export :start))

(in-package :um)

;;; Globals

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *operators* (make-hash-table)
    "Holds all the primitive operators defined via `defop'."))

(deftype u8  () '(unsigned-byte 8))
(deftype u32 () '(unsigned-byte 32))

;;; Heap

(defconstant +heap-initial-size+ 8192)

;;; Macros

(defmacro :reg (idx)         `(the u32 (aref %registers% ,idx)))
(defmacro :op  (instruction) `(ldb (byte 4 28) ,instruction))
(defmacro :a   (instruction) `(ldb (byte 3 6) ,instruction))
(defmacro :b   (instruction) `(ldb (byte 3 3) ,instruction))
(defmacro :c   (instruction) `(ldb (byte 3 0) ,instruction))
(defmacro :mem (pos)         `(aref %heap% ,pos))

(defmacro mod32 (int)
  `(logand #xFFFFFFFF ,int))

(defmacro defop (opcode name &rest body)
  "Lexically defines a primitive operator.

The body of the operator is stored in *operators* and
will be lexically inserted inside `fetch-decode-execute'
by having `assemble' create a dispatch table. It will thus
have access to the lexical environment introduced
in `fetch-decode-execute'."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,opcode *operators*)
           (list ',name '(progn ,@body)))))

;;; Operators

(defop 0 CMOV (unless (= (:reg c) 0) (setf (:reg a) (:reg b))))

(defop 1 ARRIDX (setf (:reg a) (aref (:mem (:reg b))
                                     (:reg c))))

(defop 2 ARRSTOR (setf (aref (:mem (:reg a)) (:reg b))
                       (:reg c)))

(defop 3 ADD (setf (:reg a) (mod32 (+ (:reg b) (:reg c)))))

(defop 4 MUL (setf (:reg a) (mod32 (* (:reg b) (:reg c)))))

(defop 5 DIV (setf (:reg a) (mod32 (truncate (:reg b) (:reg c)))))

(defop 6 NAND (setf (:reg a) (mod32 (lognand (:reg b) (:reg c)))))

;; Special operators

(defop 7 HALT (setf flags 666))

(defop 8 ALLOC
  (unless %free-list%
    (let* ((len (length %heap%))
           (new (make-array (* len 2))))
      (loop :for i :across %heap% :for k :upfrom 0 :do (setf (aref new k) i))
      (loop :for i :from len :to (1- (* len 2)) :do
        (locally (declare (type fixnum i))
          (setf %free-list% (cons i %free-list%))))
      (setf %heap% new)
      (format *error-output* "; Heap: ~A~%" (* len 2))))
  (let ((idx (pop %free-list%))
        (arr (make-array (:reg c) :initial-element 0 :element-type 'u32)))
    (setf (:reg b) idx
          (:mem idx) arr)))

(defop 9 FREE
  (setf %free-list% (cons (:reg c) %free-list%)))

(defop 10 OUTP
  (write-char (code-char (logand #xFF (:reg c))))
  (force-output))

(defop 11 INP
  (setf (:reg c) (handler-case (char-code (read-char))
                   (end-of-file () #xFFFFFFFF))))

(defop 12 LOAD
  (setf pc (:reg c))
  (let ((rb (:reg b)))
    (declare (type u32 rb))
    (unless (zerop rb)
      (let ((dup (copy-seq (:mem rb))))
        (setf (:mem 0) dup
              rom dup)))))

(defop 13 REGLOAD
  (setf (:reg (ldb (byte 3 25) inst))
        (ldb (byte 25 0) inst)))

;;; Utility functions

(defun read-program (program-file)
  (with-open-file (stream program-file :element-type 'u8)
    (let* ((len (truncate (file-length stream) 4))
           (array (make-array len :element-type 'u32)))
      (declare (type (simple-array u32 (*)) array))
      (format t "; Loading..~%")
      (loop :with start = (get-internal-real-time)
            :repeat len
            :for b1 = (read-byte stream) :for b2 = (read-byte stream)
            :for b3 = (read-byte stream) :for b4 = (read-byte stream)
            :for idx :upfrom 0 :do
              (setf (aref array idx)
                    (logior (ash b1 24) (ash b2 16) (ash b3 8) b4))
            :finally
               (format t "; Read ~A bytes in ~A secs from ~A~%"
                       (* len 4)
                       (float (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second))
                       program-file))
      array)))

(defmacro assemble (opc)
  "Create a dispatch table based on the operators we have defined with `defop'."
  `(case ,opc
     ,@(loop
         :with values = '()
         :for op :being :the :hash-keys :in *operators*
         :using (hash-value v)
         :for code = (second v)
         :unless (= op 13)
         :do (push (list op code) values)
         :finally (return (sort values #'< :key #'first)))
     (otherwise (error "Unknown opcode: ~A" ,opc))))

(defmacro fetch-decode-execute (program)
  `(let* ((pc 0) (flags 0) (a 0) (b 0) (c 0) (opc 0)
          (inst 0)
          (%free-list% (loop :for i :from 1 :to (1- +heap-initial-size+) :collect i))
          (%registers% (make-array 8 :element-type 'u32 :initial-element 0))
          (%heap% (make-array +heap-initial-size+))
          (rom ,program))
     (declare (type u32 pc inst)
              (type fixnum flags)
              (type (unsigned-byte 3) a b c)
              (type (unsigned-byte 4) opc)
              (type (simple-array u32 (*)) rom)
              (type (simple-array u32 (8)) %registers%)
              (type (simple-array (simple-array u32 1) 1) %heap%)
              ;; try and stack allocate the registers array
              (dynamic-extent %registers%))
     (setf (:mem 0) rom)
     ;; Interpreter loop
     (loop :while (/= flags 666) :do
       (setf inst (aref rom pc)
             opc (:op inst)
             pc (mod32 (1+ pc)))
       (if (= 13 opc)
           ,(second (gethash 13 *operators*))
           (progn (setf a (:a inst)
                        b (:b inst)
                        c (:c inst))
                  (assemble opc))))))

;;; Main

(defun start (program-file)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 1)
           (compilation-speed 0)))
  (let ((data (read-program program-file)))
    (time (fetch-decode-execute data))))

(start "scrolls/sandmark.umz")
