#lang sicp

; fast-mult: Returns the product of integers a and b
; in O(log a) time and space.
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
(define (test-fast-mult a b maxb)
  (cond ((<= b maxb)
         (display (fast-mult a b))
         (newline)
         (test-fast-mult a (+ b 1) maxb))))
(test-fast-mult 7 0 11)