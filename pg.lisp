























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

(funcall x '(a b c) '(d e f))


(setf (symbol-function 'triple)
      #'(lambda (x) (* x 3)))

(triple 3)
