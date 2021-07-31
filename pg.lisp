























(defun double (x) (* x 2))


(double 1)

#'double

(setq double 2)

(double double)

(symbol-value 'double)

(symbol-function 'double)


(setq x #'append)

(eq (symbol-value 'x)
    (symbol-function 'append))


(append '(a b c) '(d e f))


;; x would either have to be apply'd or funcall'd in order to use it as a function
(funcall x '(a b c) '(d e f))

(apply x '((a b c) (d e f)))


(setf (symbol-function 'triple)
      #'(lambda (x) (* x 3)))

(triple 3)


(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))

(defun make-adderc (n)
  (lambda (x &optional change)
    (if change
        (setq n x)
        (+ x n))))

(setq addx (make-adderb 1))

(setq addy (make-adderc 1))


(funcall addy 16 t)

(funcall addy 4)

(defun foo (x) (1+ x))

(compiled-function-p #'foo)

(compile 'foo)


(nconc '(hi) '(there))
(setq x '(a b c))
(setq y '(d e f))

(nconc x y)

x

;; rplaca - replaces the car of the cons object
;; rplacd - replaces the cdr of the cons object

(setq rc (cons 2 3))

(rplaca rc 8)

rc

(rplacd rc 12)

rc

(multiple-value-bind (int frac) (truncate 26.21875)
  (list int frac))

(defun ok (x)
  (nconc (list 'a x) (list 'c)))

(ok 5)

(setq eh '(18))

(ok eh)

eh

(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))

(setq hb '(4))

hb

(not-ok hb)

hb


(append eh '(oh my))

eh

(defun exclaim (expression)
  (append expression '(oh my)))

(exclaim '(lions and tigers and bears))

(nconc * '(goodness))
