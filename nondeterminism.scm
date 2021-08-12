



(define *paths* ())
(define failsym '@)


(define (choose choices)
  (if (null? choices)
      (fail)
      (call-with-current-continuation
       (lambda (cc)
         (set! *paths*
               (cons (lambda ()
                       (cc (choose (cdr choices)))) ;; in simpler examples (continuity.scm)
                     ;; cc was stored and called later with an integer/symbol argument
                     ;; now we are calling it with a recursive function call on cdr
                     *paths*))
         (car choices)))))
