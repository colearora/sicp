#lang sicp

;===============================================================================
; Source
;===============================================================================

; Set implementation: unordered list.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;===============================================================================
; Test
;===============================================================================

(define set1 '(3 1 4 5 9 2 6))
(define set2 '(1 3 5 7 9))
(define set3 '(8))
(define set4 '())

(element-of-set? 7 set1) ; false
(element-of-set? 7 set2) ; true

(adjoin-set 6 set2) ; (6 1 3 5 7 9)
(adjoin-set 6 (adjoin-set 6 set2)) ; (6 1 3 5 7 9)

(intersection-set set1 set2) ; (3 1 5 9)
(intersection-set set2 set3) ; ()

(union-set set1 set2) ; (4 2 6 1 3 5 7 9)
(union-set set3 set4) ; (8)
(union-set set4 set3) ; (8)

