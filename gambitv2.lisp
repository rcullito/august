(defvar *small-numbers*
  (loop for n from 2 to 99
        collect n))

(defmacro gambit (operator)
  `(progn ,@(mapcar (lambda (baked-in-n)
                      `(defun ,(symb operator baked-in-n)
                           (x)
                         (,operator x ,baked-in-n)))
                    *small-numbers*)))

;; exposes predicates
(gambit >)
(gambit <)
