#lang sicp

;===============================================================================
; Source
;===============================================================================
  
(define (cons a b)
  (* (fast-expt 2 a)
     (fast-expt 3 b)))
(define (car z) (times-divisible z 2))
(define (cdr z) (times-divisible z 3))

; times-divisible returns the exponent of the highest power of
; d that divides z. For example, (times-divisible 18 3) returns 2.
(define (times-divisible z d)
  (define (iter count rem)
    (if (divides? d rem)
        (iter (+ count 1) (/ rem d))
        count))
  (iter 0 z))

; fast-expt returns b^n.
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (divides? x y) (= (remainder y x) 0))
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))

;===============================================================================
; Test
;===============================================================================

(car (cons 42 3)) ; 42
(cdr (cons 42 3)) ; 3

(times-divisible 14 2) ; 1
(times-divisible 18 3) ; 2
