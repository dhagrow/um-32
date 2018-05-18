(defconstant +buffer-size+ 4096)

(defun read-uint32 (stream)
  (loop repeat 4
        for offset from 0 by 8
        for value = (read-byte stream nil)
          then (logior (ash (read-byte stream) offset) value)
        finally (return value)))

(defun load-um (filename)
  (with-open-file (fp filename :direction :input
                      :element-type '(unsigned-byte 8))
    (loop for platter = (read-uint32 fp)
          while platter
          do (print platter))))

;; (load-um "scrolls/sandmark.umz")

;; recursive-p ?
(with-open-file (stream "docs/key.txt" :direction :input
  (loop for line = (read-line stream nil nil)
        while line
        do (print line) )
