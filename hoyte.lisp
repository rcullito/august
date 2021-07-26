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

(flatten '(1 2 (3 4) 5))

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


(segment-reader t #\/ 3)


(char= #\a #\b)


