#lang sicp

; Interval data type.
; Supports interval arithmetic.

;===============================================================================
; Source
;===============================================================================

; Constructors:

; make-interval creates a new interval x from numerical bounds a and b.
; Maintains the invariant that (lower-bound x) <= (upper-bound x).
(define (make-interval a b)
  (if (<= a b) (cons a b) (cons b a)))

; make-center-width takes a center c and width w, and returns the
; interval spanning from c-w to c+w.
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

; make-center-percent takes a center c and a tolerance tol, where tol
; is the ratio of interval width to (absolute value of) center as a percent,
; and returns the corresponding interval.
(define (make-center-percent c tol)
  (make-center-width c (abs (* c (/ tol 100.0)))))

; Selectors:

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; center returns the center of interval x, which is the average of x's bounds.
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2.0))

; width returns half of the difference between the upper and lower bounds
; of interval x. The width is a measure of the uncertainty of the number
; specified by the interval.
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

; percent returns the percentage tolerance of interval x, defined as the
; ratio of the interval's width to the absolute value of its center.
(define (percent x)
  (* (/ (width x) (abs (center x)))
     100.0))
  
; Operations:

; print-interval takes an interval x with lower and upper bounds
; a and b respectively, and prints "(a, b)".
(define (print-interval x)
  (display "(")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display ")\n"))

; print-center-percent takes an interval x with center c and
; percent tolerance t, and prints "c (%t)".
(define (print-center-percent x)
  (display (center x))
  (display " (")
  (display (percent x))
  (display "%)\n"))

; reciprocal returns the reciprocal of interval x.
; This is a new interval whose bounds are the reciprocals
; of those in x.
(define (reciprocal x)
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))

; contains returns true if interval x contains val, false otherwise.
(define (contains x val)
  (and (>= val (lower-bound x))
       (<= val (upper-bound x))))

; add-interval returns the sum of the intervals x and y.
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; sub-interval returns the difference of the intervals x and y.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; mul-interval returns the product of intervals x and y.
(define (mul-interval x y)
  (let ((xa (lower-bound x))
        (xb (upper-bound x))
        (ya (lower-bound y))
        (yb (upper-bound y)))
    (cond ; case analysis based on signs of bounds of x and y
      ; x=(+,+), y=(+,+) (note: + here and below means >= 0)
      ((and (non-neg? xa) (non-neg? xb) (non-neg? ya) (non-neg? yb))
       (make-interval (* xa ya) (* xb yb)))
      ; x=(+,+), y=(-,+)
      ((and (non-neg? xa) (non-neg? xb) (neg? ya) (non-neg? yb))
       (make-interval (* xb ya) (* xb yb)))
      ; x=(+,+), y=(-,-)
      ((and (non-neg? xa) (non-neg? xb) (neg? ya) (neg? yb))
       (make-interval (* xb ya) (* xa yb)))
      ; x=(-,+), y=(+,+)
      ((and (neg? xa) (non-neg? xb) (non-neg? ya) (non-neg? yb))
       (make-interval (* xa yb) (* xb yb)))
      ; x=(-,+), y=(-,+)
      ((and (neg? xa) (non-neg? xb) (neg? ya) (non-neg? yb))
       (make-interval (min (* xa yb) (* xb ya))
                      (max (* xa ya) (* xb yb)))
      ; x=(-,+), y=(-,-)
      ((and (neg? xa) (non-neg? xb) (neg? ya) (neg? yb))
       (make-interval (* xb ya) (* xa ya)))
      ; x=(-,-), y=(+,+)
      ((and (neg? xa) (neg? xb) (non-neg? ya) (non-neg? yb))
       (make-interval (* xa yb) (* xb ya)))
      ; x=(-,-), y=(-,+)
      ((and (neg? xa) (neg? xb) (neg? ya) (non-neg? yb))
       (make-interval (* xa yb) (* xa ya)))
      ; x=(-,-), y=(-,-)
      ((and (neg? xa) (neg? xb) (neg? ya) (neg? yb))
       (make-interval (* xb yb) (* xa ya)))))))
           
; div-interval returns the quotient of intervals x and y.
; An error is thrown if interval y spans zero.
(define (div-interval x y)
  (if (contains y 0)
      (error "div-interval: divisor spans zero"))
  (mul-interval x (reciprocal y)))

; Helper procedures:

(define (neg? x) (< x 0))
(define (non-neg? x) (>= x 0))

;===============================================================================
; Test
;===============================================================================

(define x (make-interval 2 3))
(define y (make-interval -2 2))

(print-interval x) ; (2, 3)
(print-center-percent x) ; 2.5 (20%)
(print-interval y) ; (-2, 2)
; (print-center-percent y) ; error: /: division by zero

(lower-bound x) ; 2
(upper-bound x) ; 3

(print-interval (reciprocal x)) ; (0.3333333333333333, 0.5)
(print-interval (reciprocal y)) ; (-0.5, 0.5)

(print-interval (add-interval x x)) ; (4, 6)
(print-interval (sub-interval x x)) ; (-1, 1)
(print-interval (mul-interval x y)) ; (-6, 6)
; (print-interval (div-interval x y)) ; error: div-interval: divisor spans zero

(width x) ; 0.5
(width y) ; 2.0
(width (add-interval x y)) ; 2.5 (for intervals, width of sum is sum of widths)
(width (sub-interval x y)) ; 2.5 (for intervals, width of difference is sum of widths)
(width (mul-interval x y)) ; 6.0 (no such pattern for multiplication/division)

(define r1 (make-center-percent 6.8 10))
(define r2 (make-center-percent 8.1 5))

(print-center-percent r1) ; 6.8 (9.999999999999996%)
(print-interval r1) ; (6.12, 7.4799999999999995)
(print-center-percent r2) ; 8.1 (4.999999999999997%)
(print-interval r2) ; (7.694999999999999, 8.504999999999999)

; Given two intervals with small tolerances, the tolerance of the product
; is approximately the sum of the individual tolerances.
(percent (mul-interval r1 r1)) ; 19.8019801980198 ~= 10 + 10
(percent (mul-interval r1 r2)) ; 14.925373134328357 ~= 10 + 5
(percent (mul-interval r2 r2)) ; 9.975062344139651 ~= 5 + 5
(percent (mul-interval r1 (make-interval 2 2))) ; 9.999999999999996 ~= 10 + 0

; The tolerance of the result of adding an interval x to itself
; is just the tolerance of x. The relationship is more complicated
; when the intervals are distinct.
(percent (add-interval r1 r1)) ; 9.999999999999996 - equal to tolerance of r1
(percent (add-interval r1 r2)) ; 7.281879194630873 - between tolerances of r1 and r2
(percent (add-interval r1 (make-interval 2 2))) ; 7.727272727272723 - between

; The results of interval arithmetic can differ between expressions that are
; algebraically equivalent. Example:
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1)
                                    (div-interval one r2)))))
(print-center-percent (par1 r1 r2)) ; 3.775525304158316 (21.96848833176435%)
(print-center-percent (par2 r1 r2)) ; 3.6943392573860465 (7.727479872398593%)

; Among equivalent algebraic expressions the expression that minimizes the
; number of uncertain quantities (i.e., intervals) should have the lowest tolerance
; for two reasons: (1) multiplication/division of intervals increases tolerance while
; multiplication/division of an interval with a known quantity (or interval with
; 0% tolerance) does not; and (2) certain binary interval operations involving an
; interval with itself have attached uncertainty where none should exist, e.g., 
(print-interval (div-interval r1 r1)) ; (0.8181, 1.22) - should be 1 (0%)
(print-interval (sub-interval r1 r1)) ; (-1.359, 1.359) - should be 0 (0%)


