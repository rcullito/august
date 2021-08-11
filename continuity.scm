(define (foo x) (1+ x))


(define frozen)

;; call-with-current-continuation takes a function of one argument,
;; that argument will represent the current continuation

;; by storing the value of that continuation, we save the state of computation
;; at the point of the outer call-with-current-continuation

(append '(the continuation example returned)
        (list (call-with-current-continuation
               (lambda (cc)
                 (set! frozen cc)
                 'a))))

(frozen 'toast)


(1+ (frozen 'meadow))
