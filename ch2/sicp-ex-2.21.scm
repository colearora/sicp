#lang sicp

;===============================================================================
; Source
;===============================================================================

; square-list takes a list of numbers as input and returns a list
; of the squares of those numbers.
(define (square-list items)
  (square-list-1 items))

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

; map takes a procedure and list of items as inputs, applies the procedure
; to each item, and returns a list of the results.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))

;===============================================================================
; Test
;===============================================================================

(square-list-1 (list 1 2 3 4)) ; (1 4 9 16)
(square-list-2 (list 1 2 3 4)) ; (1 4 9 16)