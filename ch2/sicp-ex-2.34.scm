#lang sicp

;===============================================================================
; Source
;===============================================================================

; horner-eval takes as input a number x and coeff-seq which is a list of
; coefficients (a_0, a_1, ..., a_n) and returns the value of the polynomial
; a_n x^n + ... + a_1 x + a_0 at the given value of x.
(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coeff-seq))

; accumulate returns the result of successively applying op pairwise
; to the elements of seq from back to front with the first application
; between the last element of seq and init.
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

;===============================================================================
; Test
;===============================================================================

(horner-eval 2 (list 7 1 -2 1)) ; 1(2^3) + (-2)(2^2) + 1(2^1) + 7(2^0) = 9