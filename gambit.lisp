


(defparameter *small-numbers*
  (loop for n from 2 to 99
        collect n))

(defun symbol-builder (n)
  (values (intern (concatenate 'string
                               (write-to-string n)
                               (make-string 1 :initial-element #\+)))))

;; TODO do this for +, - * /

(defmacro gambit ()
  `(progn ,@(mapcar (lambda (n)
             `(defun ,(symbol-builder n) (x) (+ ,n x)))
           *small-numbers*)))

;; (macroexpand '(gambit))

(gambit)

(75+ 74)

(4+ 8)



;; (75+ 4)

;; (2+ 2)

