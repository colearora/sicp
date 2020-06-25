#lang sicp

; fast-mult: returns the product of integers a and b
; in O(log n) time and O(1) space.
(define (fast-mult a b)
  (define (mult-iter a b c) ; maintains invariant a*b + c
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
(define (fast-mult-test a b maxb)
  (cond ((<= b maxb)
         (display (fast-mult a b))
         (newline)
         (fast-mult-test a (+ b 1) maxb))))
(fast-mult-test 7 0 11)