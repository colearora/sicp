#lang sicp

; fast-expt: raises base b to integer exponent n
; in O(n) time and O(1) space.
(define (fast-expt b n)
  (define (expt-iter a b n) ; maintains invariant quantity a * b^n
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (expt-iter 1 b n))

(define (square x)
  (* x x))
(define (even? x)
  (= (remainder x 2) 0))

; Test:
(define (test-fast-expt b m n)
  (cond ((< m n)
         (display (fast-expt b m))
         (newline)
         (test-fast-expt b (+ m 1) n))))
(test-fast-expt 3 0 10)
