#lang sicp

;===============================================================================
; Source
;===============================================================================
  
(define zero (lambda (f) (lambda (x)       x)))
(define one  (lambda (f) (lambda (x)    (f x))))
(define two  (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (+ n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

;===============================================================================
; Test
;===============================================================================

(define three (add-1 two))
(define six (+ three three))