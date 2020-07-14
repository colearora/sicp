#lang sicp

;===============================================================================
; Source
;===============================================================================

; filtered-accumulate applies term to each input in the sequence
; a, (next a), (next (next a)), ..., not exceeding b, for which
; the filter predicate returns true, and returns the combination
; of the results.
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a res)
    (cond ((> a b) res)
          ((filter a) (iter (next a) (combiner res (term a)))) ; accumulate a
          (else (iter (next a) res)))) ; skip a
  (iter a null-value))

; sum-of-squares-of-primes returns the sum of the squares
; of the prime numbers in [a..b].
(define (sum-of-squares-of-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

; product-of-relatively-prime returns the product of all positive
; integers i < n for which GCD(i, n) = 1.
(define (product-of-relatively-prime n)
  (define (relatively-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc (- n 1)))

(define (prime? n)
  (= (smallest-divisor n) n))
(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (next test-divisor)))))
  (define (next x)
    (if (= x 2) 3 (+ x 2)))
  (find-divisor 2))
(define (divides? x y) (= (remainder y x) 0))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

;===============================================================================
; Test
;===============================================================================

(sum-of-squares-of-primes 2 13) ; 377
(+ (square 2) (square 3) (square 5) (square 7) (square 11) (square 13)) ; 377

(product-of-relatively-prime 15) ; 896896
(* 2 4 7 8 11 13 14) ; 896896

