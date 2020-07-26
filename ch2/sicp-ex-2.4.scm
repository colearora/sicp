#lang sicp

;===============================================================================
; Source
;===============================================================================
  
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
  
;===============================================================================
; Test
;===============================================================================

(car (cons 42 3)) ; 42
(cdr (cons 42 3)) ; 3
