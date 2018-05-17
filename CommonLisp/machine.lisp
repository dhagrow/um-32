(defconstant +buffer-size+ 4096)

(defun load-um (filename)
  (with-open-file (in filename :direction :input
                                  :element-type '(unsigned-byte 8))
    (loop with buffer = (make-array +buffer-size+
                                    :element-type (stream-element-type in))
          for size = (read-sequence buffer in)
          while (plusp size)
          do (print buffer))))

(load-um "scrolls/sandmark.umz")
