#lang sicp

;===============================================================================
; Source
;===============================================================================

; Rational number operations.
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; make-rat returns a rational number whose numerator and denominator
; are n and d respectively.
(define (make-rat n d)
  ; Handle zero cases.
  (cond ((= d 0) (error "make-rat: divide-by-zero"))
        ((= n 0) (cons 0 1)))

  ; Handle general cases.
  ; Sign is normalized such that if the rational number is negative,
  ; the numerator carries the negative sign.
  ; Both numerator and denominator are divided by their GCD such that
  ; the rational number is in lowest terms.
  (let ((g (gcd (abs n) (abs d))))
    (cond ((> d 0) (cons (/ n g) (/ d g)))
          (else (cons (/ (- n) g) (/ (- d) g))))))

; numer returns the numerator of rational number x.
(define (numer x) (car x))

; denom returns the denominator of rational number x.
(define (denom x) (cdr x))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

;===============================================================================
; Test
;===============================================================================

(define one-half (make-rat 1 2))
(print-rat one-half) ; 1/2
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third)) ; 5/6
(print-rat (mul-rat one-half one-third)) ; 1/6
(print-rat (add-rat one-third one-third)) ; 2/3
(newline)
(print-rat (make-rat -1 2)) ; -1/2
(print-rat (make-rat -3 6)) ; -1/2
(print-rat (make-rat 1 -2)) ; -1/2
(print-rat (make-rat 4 -8)) ; -1/2
(equal-rat? (make-rat -1 2) (make-rat 1 -2)) ; true
(equal-rat? (make-rat -3 6) (make-rat 4 -8)) ; true
