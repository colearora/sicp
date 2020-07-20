#lang sicp

;===============================================================================
; Source
;===============================================================================

; compose returns a single-argument procedure that is the
; composition of procedures f and g.
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

;===============================================================================
; Test
;===============================================================================

((compose square inc) 6) ; 49




