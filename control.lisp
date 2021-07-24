


(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))


;; question 1 how does it work flopping the outer form, in this case the mapcar on line 5
;; question 2, explore cadr in thise case
