(defvar memory)
(defvar reg)

(setf memory (list '()))
(setf reg (make-array '(8) :initial-element 0))

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
        (if (eql code 13) (let (
          (a (logand (ash platter -25) 7))
          (val (logand platter #x1ffffff)))
          (progn
            ;; (format t "finger=~d reg=~d~%" finger reg)
            ;; (format t "code=~d a=~d val=~d~%" code a val)
            (setf (aref reg a) val)))
        (let (
          (a (logand (ash platter -6) 7))
          (b (logand (ash platter -3) 7))
          (c (logand platter 7)))
          (progn
            ;; (format t "finger=~d reg=~d~%" finger reg)
            ;; (format t "code=~d a=~d b=~d c=~d~%" code a b c)
            (ccase code
            (0 (if (not (eql (aref reg c) 0))
              (setf (aref reg a) (aref reg b))))
            (3 (setf (aref reg a) (mod (+ (aref reg b) (aref reg c)) (expt 2 32))))
            (4 (setf (aref reg a) (mod (* (aref reg b) (aref reg c)) (expt 2 32))))
            (5 (setf (aref reg a) (floor (aref reg b) (aref reg c))))
            (6 (setf (aref reg a) (boole boole-nand (aref reg b) (aref reg c))))
            (10 (format t "~d" (code-char (aref reg c))))
            (12 (progn
              (if (not (eql (aref reg b) 0))
                (setf (nth 0 memory) (copy-list (nth (aref reg b) memory))))
              (setq finger (- (aref reg c) 1))))))))
        (incf finger)))))

(load-um "scrolls/sandmark.umz")
(run)
;; (print program)
;; format t "~32,'0b~%" platter
;; format t "code=~d a=~d b=~d c=~d~%" code a b c
