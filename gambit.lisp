


(defparameter *small-numbers*
  (loop for n from 2 to 99
        collect n))

(defun symbol-builder (n)
  (values (intern (concatenate 'string
                               (write-to-string n)
                               (make-string 1 :initial-element #\+)))))

(values (intern "bob"))
;; values returns the objects as multiple values
(values 1 2 3)

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

;; won't work because the name of a function must be a symbol
;; not an s-expression. ok, need a macro!!
;; (progn (mapc (lambda (n)
;;                (defun (symbol-builder n) (x) (+ n x)))
;;         *small-numbers*))
