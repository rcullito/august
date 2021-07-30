(load "prerequisites.lisp")


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


(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))


(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d) ;; d is like the row
            `(,(if (eq t (car d))
                   t
                   (list (car d))) ;; basically the matching part of our case
              (apply (lambda ,@(cdr d)) ;; will give both the args and the function body
                     ,(if (eq t (car d))
                          g!args ;; no idea what this last bit does
                          `(cdr ,g!args)))))
          ds))))


(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(funcall
 (alambda (n)
   (if (> n 0)
       (cons n
             (self (1- n))))) 8)


;; read macro

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
             collect (symb 'a i)) ;; so we are exposing a lambda with a1, a2, etc... exposed
     ,(funcall
       (get-macro-character #\`) stream nil)))


;; causes a new function to be called when disp-character followed by sub-char is read
(set-dispatch-macro-character
  #\# #\` #'|#`-reader|)


'#`((,a1))



(mapcar (lambda (a)
          `(,a 'empty))
        '(var-a var-b var-c))

(expt 2 2)

(expt 2 4)

(expt 2 0.5)
