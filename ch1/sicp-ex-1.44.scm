#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (smooth f)
  (let ((dx 0.1))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3.0))))

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

(define (identity x) x)
(define (square x) (* x x))
(define (inc x) (+ x 1))

;===============================================================================
; Test
;===============================================================================

(define (test-n-fold-smooth f lo hi step)
  (define (iter x)
    (cond ((<= x hi)
           (display "x=")
           (display x)
           (display ", f(x)=")
           (display (f x))
           (display ", s(f)(x)=")
           (display ((smooth f) x))
           (display ", s^10(f)(x)=")
           (display ((n-fold-smooth f 10) x))
           (newline)
           (iter (+ x step)))))
  (iter lo))
(test-n-fold-smooth square 0.0 5.0 0.1)




