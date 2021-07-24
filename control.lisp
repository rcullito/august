


(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))


(defun nlet-fact (n)
  (nlet fact
        ((n n))
        (if (zerop n)
            1
            (* n (fact (1- n))))))

(nlet-fact 5)



(macroexpand '(nlet fact
       ((n n))
       (if (zerop n)
           1
           (* n (fact (1- n))))))

;; question 1 how does it work flopping the outer form, in this case the mapcar on line 5
;; I guess it means start running the code at this point

;; question 2, explore cadr in thise case
;; we map over each of the bindings, so the individuall mapped binding is '(n n)
;; cdr would get (n), cadr will get just n
;; and then because we mapcar'd we are returned a list, and we need ,@ to "explode" it


(mapcar #'cadr '((n n)))
