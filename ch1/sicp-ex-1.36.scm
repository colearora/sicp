#lang sicp

;===============================================================================
; Source
;===============================================================================

(define tolerance 0.00001)

; fixed-point-verbose computes a fixed point of function f
; using successive approximation starting from first-guess.
; Each approximation is printed.
(define (fixed-point-verbose f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (print-step guess step)
    (display step)
    (display ": ")
    (display guess)
    (newline))
  (define (try guess step) ; step counts number of calls to try
    (let ((next (f guess)))
      (print-step next step)
      (cond ((close-enough? guess next)
             next)
            (else (try next (+ step 1))))))
  (print-step first-guess 0)
  (try first-guess 1))

(define (avg x y) (/ (+ x y) 2))

;===============================================================================
; Test
;===============================================================================

; Without average damping: converges in 37 steps.
(fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 1.1)

; With average damping: converges in 13 steps.
(fixed-point-verbose (lambda (x) (avg x (/ (log 1000) (log x)))) 1.1)

