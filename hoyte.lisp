(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (print a s))))

(defun symb (&rest args)
  ;; values returns the objects as multiple values
  (values (intern (apply #'mkstr args))))

(symb '1 '+)

(defun group (source n)
  (if (zerop n)
      (error "zero length"))
  (labels ((rec (source acc)
                 (let ((rest (nthcdr n source)))
                   (if (consp rest)
                       (rec rest (cons ;; so we are cons'ing the next 2, say, and then running
                                       ;; through the entire source again
                                  (subseq source 0 n)
                                  acc))
                       (nreverse ;; we have been cons'ing later elements to the
                        ;; front of the list, so we need to reverse
                        (cons source acc))))))
    (if source (rec source nil) nil)))



(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc)))))) ;; beware, double call to rec within one form!
    (rec x nil))) 


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


(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))



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

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))



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


;; (defmacro/g! nif (expr pos zero neg)
;;   `(let ((,g!result ,expr))
;;      (cond ((plusp ,g!result) ,pos)
;;            ((zerop ,g!result) ,zero)
;;            (t ,neg))))

;; (nif 25 'pos 'zero 'neg)

;; (gensym "result")


;; (segment-reader t #\/ 3)


;; (char= #\a #\b)


