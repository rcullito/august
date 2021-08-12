



(define *paths* ())
(define failsym '@)


(define (choose choices)
  (if (null? choices)
      (fail)
      (call-with-current-continuation ;; this is where we will jump back to
       ;; so if we have some choices, try the first one, but save the possibility
       ;; of caling the rest in paths
       (lambda (cc)
         (set! *paths*
               (cons (lambda ()
                       (cc (choose (cdr choices)))) ;; in simpler examples (continuity.scm)
                     ;; cc was stored and called later with an integer/symbol argument
                     ;; now we are calling it with a recursive function call on cdr
                     *paths*))
         (car choices)))))



(define fail)

(call-with-current-continuation
 (lambda (cc)
   (set! fail
         (lambda ()
           (if (null? *paths*)
               (cc failsym)
               ;; if we failed but have more options
               ;; try the first one
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))
