(defvar memory)
(defvar reg)

(setf memory (list '()))
(setf reg (make-array '(8) :element-type '(unsigned-byte 32) :initial-element 0))

(defun read-uint32 (stream)
  (logior (ash (read-byte stream) 24)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 8)
          (read-byte stream)))

(defun load-um (filename)
  (setf (nth 0 memory) '())
  (with-open-file (stream filename :direction :input
                          :element-type '(unsigned-byte 8))
    (loop for platter = (ignore-errors (read-uint32 stream))
          while platter
          do (setf (nth 0 memory) (append (nth 0 memory) (list platter))))))

(defun run ()
  (let ((finger 0))
    (loop
      for platter = (nth finger (nth 0 memory))
      for code = (ash platter -28)
      do (progn
        (if (eql code 13)
          (let ((a (logand (ash platter -25) 7))
                (val (logand platter #x1ffffff)))
            ;; (format t "finger=~d reg=~d~%" finger reg)
            ;; (format t "code=~d a=~d val=~d~%" code a val)
            (setf (aref reg a) val))
        (let ((a (logand (ash platter -6) 7))
              (b (logand (ash platter -3) 7))
              (c (logand platter 7)))
          ;; (format t "finger=~d reg=~d~%" finger reg)
          ;; (format t "code=~d a=~d b=~d c=~d~%" code a b c)
          (ccase code
          (0 (if (not (eql (aref reg c) 0))
            (setf (aref reg a) (aref reg b))))
          (1 (setf (aref reg a) (nth (aref reg c) (nth (aref reg b) memory))))
          (2 (setf (nth (aref reg b) (nth (aref reg a) memory)) (aref reg c)))
          (3 (setf (aref reg a) (mod (+ (aref reg b) (aref reg c)) (expt 2 32))))
          (4 (setf (aref reg a) (mod (* (aref reg b) (aref reg c)) (expt 2 32))))
          (5 (setf (aref reg a) (floor (aref reg b) (aref reg c))))
          (6 (setf (aref reg a) (mod (lognand (aref reg b) (aref reg c)) (expt 2 32))))
          (10 (format t "~d" (code-char (aref reg c))))
          (12 (progn
            (if (not (eql (aref reg b) 0))
              (setf (nth 0 memory) (copy-list (nth (aref reg b) memory))))
            (setq finger (- (aref reg c) 1)))))))
        (incf finger)))))

(load-um "scrolls/sandmark.umz")
(run)
;; (print program)
;; format t "~32,'0b~%" platter
;; format t "code=~d a=~d b=~d c=~d~%" code a b c

;; 14:00 < chris> you can write a macro to expand this: (demacro (uint32 n) `(logand #xFFFFFFFF ,n))
;; 14:00 < chris> but the cool thing is that you can also expand at compile-time
;; 14:01 < chris> (demacro (uint32 n) (assert (eql 'fixnum (type-of n))) (logand #xFFFFFFFF n))
