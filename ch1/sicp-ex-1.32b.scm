#lang sicp

;===============================================================================
; source
;===============================================================================

; accumulate applies term to each input in the sequence
; a, (next a), (next (next a)), ..., not exceeding b,
; and returns the combination of the results.
(define (accumulate combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner res (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity x) x)
(define (inc x) (+ x 1))

;===============================================================================
; test
;===============================================================================

(sum identity 1 inc 5)     ; 15 = 1 + 2 + 3 + 4 + 5
(product identity 1 inc 5) ; 120 = 1 * 2 * 3 * 4 * 5
