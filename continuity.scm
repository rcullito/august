(define (foo x) (1+ x))


(define frozen)

(append '(the continuation example returned)
 (call-with-current-continuation
  (lambda (cc)
    (set! frozen cc)
    'a)))
