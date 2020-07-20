#lang sicp

;===============================================================================
; Source
;===============================================================================

; nth-root estimates the nth root of number x.
; Assumes n is a positive integer.
(define (nth-root x n)
  (define (f y) ; function whose fixed point is x's nth root
    (/ x (fast-expt y (- n 1))))
  (let ((num-damps 3)
        (first-guess 1.0))
    (fixed-point ((repeated average-damp num-damps) f)
                 first-guess)))

; fixed-point performs a search for a fixed point of function f
; by repeatedly applying f to first-guess until the difference
; between input and output is less than some tolerance.
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define tolerance 0.00001)

; repeated takes as inputs a single-argument procedure f
; and a positive integer n, and returns a procedure that
; computes the nth repeated application of f.
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

; compose returns a single-argument procedure that is the
; composition of procedures f and g.
(define (compose f g)
  (lambda (x) (f (g x))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; fast-expt returns b^n for any number b
; and non-negative integer n.
(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define (average x y) (/ (+ x y) 2))
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (identity x) x)
(define (inc x) (+ x 1))

;===============================================================================
; Test
;===============================================================================

(nth-root 2 1)     ; ~2 (converges with 1 damp)
(nth-root 4 2)     ; ~2 (converges with 1 damp)
(nth-root 8 3)     ; ~2 (converges with 1 damp)
(nth-root 16 4)    ; ~2 (converges with 2 damps)
(nth-root 32 5)    ; ~2 (converges with 2 damps)
(nth-root 64 6)    ; ~2 (converges with 2 damps)
(nth-root 128 7)   ; ~2 (converges with 2 damps)
(nth-root 256 8)   ; ~2 (converges with 3 damps)
(nth-root 512 9)   ; ~2 (converges with 3 damps)
(nth-root 1024 10) ; ~2 (converges with 3 damps)
(nth-root 2048 11) ; ~2 (converges with 3 damps)
(nth-root 4096 12) ; ~2 (converges with 3 damps)




