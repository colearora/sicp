#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

; accumulate returns the result of successively applying op pairwise
; to the elements of seq from back to front with the first application
; between the last element of seq and init.
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

;===============================================================================
; Test
;===============================================================================

(define list1 (list 1 2 3 4 5))
(define list2 (list 6 7 8))
(define (inc x) (+ x 1))

(accumulate + 0 list1) ; 15
(map inc list1) ; (2 3 4 5 6)
(append list1 list2) ; (1 2 3 4 5 6 7 8)
(length list1) ; 5
