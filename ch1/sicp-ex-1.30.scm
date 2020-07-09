#lang sicp

; sum applies term to each input in the sequence
; a, (next a), (next (next a)), ..., not exceeding b,
; and returns the sum of the results.
(define (sum term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (+ res (term a)))))
  (iter a 0))

(define (identity x) x)
(define (inc x) (+ x 1))

(sum identity 1 inc 5) ; 15

