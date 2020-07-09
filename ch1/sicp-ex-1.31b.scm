#lang sicp

;===============================================================================
; source
;===============================================================================

; pi-approx approximates the value of pi
; using a positive even number n of terms
; in the Wallis formula.
(define (pi-approx n)
  (define (add-2 x) (+ x 2))
  (if (not (even? n))
      (error "n must be even"))
  (* 4.0
     (/ (* (product identity 2 add-2 n)
           (product identity 4 add-2 (+ n 2)))
        (square (product identity 3 add-2 (+ n 1))))))

; factorial computes n! for integer n.
(define (factorial n)
  (if (= n 0)
      1
      (product identity 1 inc n)))

; product applies term to each input in the sequence
; a, (next a), (next (next a)), ..., not exceeding b,
; and returns the product of the results.
(define (product term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (term a)))))
  (iter a 1))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))

;===============================================================================
; test
;===============================================================================

; test-pi-approx exercises pi-approx.
(define (test-pi-approx i n)
  (cond ((<= i n)
         (display "i = ")
         (display i)
         (display ", pi = ")
         (display (pi-approx i))
         ; (display (* 1.0 (pi-approx i)))
         (newline)
         (test-pi-approx (+ i 2) n))))
(test-pi-approx 2 100)

; test-factorial prints i! for i in [0..n].
(define (test-factorial n)
  (define (iter i)
    (cond ((<= i n)
           (display i)
           (display "! = ")
           (display (factorial i))
           (newline)
           (iter (+ i 1)))))
  (iter 0))
(test-factorial 9)