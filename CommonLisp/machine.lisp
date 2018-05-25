(defvar memory)
(defvar abandoned-indexes)
(defvar reg)
(defvar op)

(setf abandoned-indexes '())
(setf reg (make-array '(8) :element-type '(unsigned-byte 32) :initial-element 0))
(setq op (make-array 14 :initial-contents '(cmv aix aam add mul dvi nad hlt alc abd out inp lod ort)))

(defmacro uint32 (n) `(logand #xFFFFFFFF ,n))

(defun read-uint32 (stream)
  (logior (ash (read-byte stream) 24)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 8)
          (read-byte stream)))

(defun load-um (filename)
  (with-open-file (stream filename :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((i 0)
          (n (floor (file-length stream) 4)))
      (setf memory (make-array 1))
      (setf (aref memory 0) (make-array n))
      (loop
        for platter = (ignore-errors (read-uint32 stream))
        while platter
        do (setf (aref (aref memory 0) i) platter)
          (incf i)))))

(defun run ()
  (let ((finger 0))
    (loop
      for platter = (aref (aref memory 0) finger)
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
            ;; (format t "code=~d a=~d b=~d c=~d~%" (aref op code) a b c)
            (ccase (aref op code)
            (cmv (if (not (eql (aref reg c) 0))
                     (setf (aref reg a) (aref reg b))))
            (aix (setf (aref reg a)
                       (aref (aref memory (aref reg b)) (aref reg c))))
            (aam (setf (aref (aref memory (aref reg a)) (aref reg b))
                       (aref reg c)))
            (add (setf (aref reg a)
                       (uint32 (+ (aref reg b) (aref reg c)))))
            (mul (setf (aref reg a)
                       (uint32 (* (aref reg b) (aref reg c)))))
            (dvi (setf (aref reg a) (floor (aref reg b) (aref reg c))))
            (nad (setf (aref reg a)
                       (uint32 (lognand (aref reg b) (aref reg c)))))
            (alc (let ((index (pop abandoned-indexes))
                       (new-array (make-array (aref reg c) :initial-element 0)))
                  (if (not (eql index nil))
                      (setf (aref memory index) new-array)
                      (progn
                        (setq index (length memory))
                        (setf memory (adjust-array memory (+ index 1)))
                        (setf (aref memory index) new-array)))
                  (setf (aref reg b) index)))
            (abd (progn (setf (aref memory (aref reg c)) nil)
                        (push (aref reg c) abandoned-indexes)))
            (out (princ (code-char (aref reg c))) (force-output))
            (lod (progn
              (if (not (eql (aref reg b) 0))
                (setf (aref memory 0) (copy-seq (aref memory (aref reg b)))))
              (setq finger (- (aref reg c) 1)))))))
        (incf finger)))))

(if (<= (list-length sb-ext:*posix-argv*) 1)
    (princ "usage: machine <source>")
    (progn
      (load-um (nth 1 sb-ext:*posix-argv*))
      (run)))

;; format t "~32,'0b~%" platter
;; format t "code=~d a=~d b=~d c=~d~%" code a b c

;; 14:00 < chris> you can write a macro to expand this: (demacro (uint32 n) `(logand #xFFFFFFFF ,n))
;; 14:00 < chris> but the cool thing is that you can also expand at compile-time
;; 14:01 < chris> (demacro (uint32 n) (assert (eql 'fixnum (type-of n))) (logand #xFFFFFFFF n))

;; 09:30 < chris> (require :sb-sprof)
;; 09:30 < chris> (sb-sprof:with-profiling (:max-samples 10000 :report :flat)
;; 09:30 < chris> (your-entry-function-here))
;; 09:31 < chris> you can just (time ..) to begin with
