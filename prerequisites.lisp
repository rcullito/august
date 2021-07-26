(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  ;; values returns the objects as multiple values
  (values (intern (apply #'mkstr args))))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc)))))) ;; beware, double call to rec within one form!
    (rec x nil)))

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

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

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
