#lang sicp

; fast-mult: returns the product of integers a and b
; in O(log n) time and space.
(define (fast-mult a b)
  (cond ((= a 0) 0)
        ((even? a) (double (fast-mult (halve a) b)))
        (else (+ b (fast-mult (- a 1) b)))))

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