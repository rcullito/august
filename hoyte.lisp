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


(defmacro! square (o!x)
  `(* ,g!x ,g!x))

(macroexpand '(square (inc x)))

(defvar x 4)
(square (incf x))


`(football-game
 (game-started-at
  #.(get-internal-real-time))
 (coin-flip
  #.(if (zerop (random 2)) 'heads 'tails)))


`(football-game
 (game-started-at
  ,(get-internal-real-time))
 (coin-flip
  ,(if (zerop (random 2)) 'heads 'tails)))




(let ((s 'hello))
  `(,s world))

(let ((s '(b c d)))
  `(a . ,s))


(let ((s '(b c d)))
  `(a ,@s e))


'(b c d)


(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
           (t ,neg))))



;; (gensym "result")


;; (segment-reader t #\/ 3)


;; (char= #\a #\b)


(defun defunits-chaining (u units prev)
  (let* ((spec (find u units :key #'car))
         (chain (cadr spec)))
    (if (listp chain)
        (* (car chain) ;; the second multiplication is about expanding out hours to minutes, etc...
           ;; all the way to the base unit
           (defunits-chaining (cadr chain) units (cons u prev)))
        chain)))

(defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val ;; the first multiplication is about the quanity supplied, ie 8 furlongs
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar
               (lambda (x)
                 `((,(car x)) ;; will put for instance km alongside m
                   ;; what do actually do with teh value falls to defunits
                   ,(defunits-chaining
                        (car x)
                        (cons
                         `(,base-unit 1)
                         (group units 2))
                      nil)))
               (group units 2))))))

;; our macro allows the user to make a custom macro
;; that itself will perform a unit calculation
;; ie defunits distance  => unit-of-distance


(defunits time s
  m 60
  h (60 m)
  d (24 h))

(unit-of-time 3 m)


(defunits distance m
  km 1000
  cm 1/100
  mm (1/10 cm))

(unit-of-distance 300 cm)

(unit-of-distance 3/4 km)

(group '(km 1000
         cm 1/100) 2)
