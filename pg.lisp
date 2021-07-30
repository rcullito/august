























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
