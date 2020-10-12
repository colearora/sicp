#lang sicp

;===============================================================================
; Source
;===============================================================================

; Set implementation: unordered list with duplicates.
; Would prefer over non-duplicate version if the use case involves
; frequent adjoin-set and union-set calls, but infrequent
; element-of-set? and intersection-set calls.

; O(n), where n is the number of items in the multiset.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; O(1), improved over O(n) for non-duplicate version.
(define (adjoin-set x set)
  (cons x set))

; O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

; O(n), improved over O(n^2) for non-duplicate version.
(define (union-set set1 set2)
  (append set1 set2))

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
(adjoin-set 6 (adjoin-set 6 set2)) ; (6 6 1 3 5 7 9)

(intersection-set set1 set2) ; (3 1 5 9)
(intersection-set set2 set3) ; ()

(union-set set1 set2) ; (3 1 4 5 9 2 6 1 3 5 7 9)
(union-set set3 set4) ; (8)
(union-set set4 set3) ; (8)

