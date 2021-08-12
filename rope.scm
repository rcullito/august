(load "nondeterminism.scm")


;; so choose will initially return 0, but
;; it will also build up *paths* with possible continuations
(define (two-numbers)
  (list (choose '(0 1 2 3 4 5))
        (choose '(0 1 2 3 4 5))))


;; meanwhile, when we encounter fail
;; we try the first of the *paths*, which will in turn be
;; a call on choose with the cdr
(define (parlor-trick sum)
  (let ((nums (two-numbers)))
    (if (= (apply + nums) sum)
        `(the sum of ,@nums)
        (fail))))


;; these two operations work in tandem until we hit the mark
(parlor-trick 7)

(parlor-trick 8)
