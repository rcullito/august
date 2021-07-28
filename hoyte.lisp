(load "prerequisites.lisp")


(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list)))
        (dolist (c data)
          (if curr
              (setq curr
                    (if (char= (car curr) c)
                        (cdr curr)
                        trig))))
        (not curr)))))


(defparameter *counter*
  (let ((counter 0))
    (lambda () (incf counter))))

(do ((x 0 (+ 2 x))
   (y 20 ( - y 2)))
   ((= x y)(- x y))
   (format t "~% x = ~d  y = ~d" x y)
)


(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream) ;; first value, how to get next value
                   (read-char stream))) ;; varlist
            ((char= ch curr)) ;; endlist, end concidtion, whatever we are reading in matches the delim
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

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

 (tagbody
   (setq val 2)
   (go lp)
   (incf val 3)
  lp (incf val 4))


(find 'a
      '(((a b) (c d)) ((c d) (b a)))
      :key #'cadadr)

(defmacro cxr% (x tree)
  (if (null x)
      tree
      `(,(cond
           ((eq 'a (cadr x)) 'car)
           ((eq 'd (cadr x)) 'cdr)
           (t (error "Non A/D symbol")))
        ,(if (= 1 (car x)) ;; so if it is just one car, we can move on to expanding the 
             ;; cdrs' because we already have our car "expanded" in front
             ;; if we were already on the cdr, then cddr in this case will get us to nil
             ;; and a return off the tree on the next recursive call. w00t
             `(cxr% ,(cddr x) ,tree)
             `(cxr% ,(cons (- (car x) 1) (cdr x)) ,tree)))))

(defun eleventh (x)
  (cxr% (1 a 10 d) x))

(cddr '(1 a 10 d))

(cddr '(10 d))

(macroexpand '(cxr% (1 a 10 d) x))

(walker:macroexpand-all
 '(cxr% (1 a 2 d) some-list))

(eleventh '(1 2 3 4 5 6 7 8 9 10 11 12 13))



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


(setf (symbol-function 'count-test)
  (let ((count 0))
    (dlambda
     (:inc () (incf count))
     (:dec () (decf count)))))

(count-test :inc)

(count-test :dec)

;; so dlambda really is just creating an anonymous function
;; yet we have different ways of tapping into this anonymous function
;; and, most importantly, from this chapter, manipulating the enclosing state/lexical context
