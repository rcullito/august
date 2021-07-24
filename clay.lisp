(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (print a s))))

(defun symb (&rest args)
  ;; values returns the objects as multiple values
  (values (intern (apply #'mkstr args))))

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


(funcall *counter*)

(multiple-value-bind (a b)
    (let ((counter 0))
      (values
       (lambda () (incf counter))
       (lambda () (decf counter))))
  (funcall b))
