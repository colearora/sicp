#lang sicp

;===============================================================================
; Source
;===============================================================================

; double takes a one-argument procedure f as input
; and returns a one-argument procedure that
; applies f to its input twice.
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

;===============================================================================
; Test
;===============================================================================

((double inc) 5) ; 7
(((double (double double)) inc) 5) ; 21




