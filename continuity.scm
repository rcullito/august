(define (foo x) (1+ x))


(define frozen)

(append '(the continuation example returned)
        (list (call-with-current-continuation
               (lambda (cc)
                 (set! frozen cc)
                 'a))))

(frozen 'toast)


(1+ (frozen 'meadow))
