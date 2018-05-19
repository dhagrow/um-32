(defun read-uint32 (stream)
  (logior (ash (read-byte stream) 24)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 8)
          (read-byte stream)))

(defun load-um (filename)
  (with-open-file (stream filename :direction :input
                      :element-type '(unsigned-byte 8))
    (loop for platter = (ignore-errors (read-uint32 stream))
          while platter
          do (print (format nil "~32,'0b" platter)))))

(load-um "scrolls/sandmark.umz")
