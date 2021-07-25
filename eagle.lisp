(defparameter *small-numbers*
  (loop for n from 2 to 99
        collect n))

(defun symbol-builder (n)
  (values (intern (concatenate 'string
                               (write-to-string n)
                               (make-string 1 :initial-element #\+)))))

