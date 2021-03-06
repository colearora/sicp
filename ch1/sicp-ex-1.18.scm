#lang sicp

; fast-mult: Returns the product of integers a and b
; in O(log a) time and O(1) space. This is the
; "Russian peasant method" of multiplication.
(define (fast-mult a b)
  (define (mult-iter a b c) ; maintains invariant quantity a*b + c
    (cond ((= a 0) c)
          ((even? a) (mult-iter (halve a) (double b) c))
          (else (mult-iter (- a 1) b (+ c b)))))
  (mult-iter a b 0))

(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))
(define (even? x)
  (= (remainder x 2) 0))

; Test
(define (test-fast-mult a b maxb)
  (cond ((<= b maxb)
         (display (fast-mult a b))
         (newline)
         (test-fast-mult a (+ b 1) maxb))))
(test-fast-mult 7 0 11)