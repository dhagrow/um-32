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
  (loop for platter in (nth 0 memory)
    for code = (ash platter -28)
    do
      (if (eql code 13) (let (
        (a (logand (ash platter -25) 7))
        (val (logand platter #x1ffffff)))
        (progn
          (format t "reg=~d~%" reg)
          (format t "code=~d a=~d val=~d~%" code a val))
          (setf (aref reg a) val))
      (let (
        (a (logand (ash platter -6) 7))
        (b (logand (ash platter -3) 7))
        (c (logand platter 7)))
        (progn
          (format t "reg=~d~%" reg)
          (format t "code=~d a=~d b=~d c=~d~%" code a b c)
          (ccase code
          (0 (if
            (not (eql (aref reg c) 0))
              (setf (aref reg a) (aref reg b))))
          (3 (setf (aref reg a) (+ (aref reg b) (aref reg c))))))))))

(load-um "scrolls/sandmark.umz")
(run)
;; (print program)
;; format t "~32,'0b~%" platter
;; format t "code=~d a=~d b=~d c=~d~%" code a b c
