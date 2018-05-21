(defvar memory)
(setq memory (list '()))

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
  (defvar a)
  (defvar b)
  (defvar c)
  (defvar val)

  (loop for platter in (nth 0 memory)
    for code = (ash platter -28)
    do
      (if (eql code 13) (progn
        (setq a (ash platter -25))
        (setq val (logand platter #x1ffffff))
        (format t "a=~d val=~d~%" a val))
      (format t "~d~%" code))))

    ;; if (eql code 13) (format t "code[13]~%")
    ;; else (format t "code[~d]~%" code)
(load-um "scrolls/sandmark.umz")
(run)
;; (print program)
;; format t "~32,'0b~%" platter
