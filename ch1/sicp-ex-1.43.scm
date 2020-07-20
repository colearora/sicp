#lang sicp

;===============================================================================
; Source
;===============================================================================

; repeated takes as inputs a single-argument procedure f
; and a positive integer n, and returns a procedure that
; computes the nth repeated application of f.
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

; compose returns a single-argument procedure that is the
; composition of procedures f and g.
(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x) x)
(define (square x) (* x x))
(define (inc x) (+ x 1))

;===============================================================================
; Test
;===============================================================================

((repeated inc 10) 2) ; 12
((repeated square 2) 5) ; 625




