#lang sicp

;===============================================================================
; Source
;===============================================================================

; subsets takes a flat list of distinct numbers s as argument, representing
; a set, and returns the set of all subsets of s. It is a recursive procedure.
; The reduction step applied to set s effectively returns the set of all
; subsets of s as the union of those that do not include the first element
; of s (rest) and those that do include the first element (the result of mapping
; each subset in rest to the same set plus that first element). The base case
; initializes the set of sets with the empty set.
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (append (list (car s)) subset))
                          rest)))))

;===============================================================================
; Test
;===============================================================================

(define s (list 1 2 3))
(subsets s) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

