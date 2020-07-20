#lang sicp

;===============================================================================
; Source
;===============================================================================

; iterative-improve takes two procedures as arguments: a method
; for telling whether a guess is good enough and a method for improving
; a guess. It returns as its value a procedure that takes a guess as
; argument and keeps improving the guess until it is good enough.
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))

; tolerance for sqrt and fixed-point:
(define tolerance 0.0001)

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

;===============================================================================
; Test
;===============================================================================

(sqrt 64) ; 8
(sqrt 121) ; 11
