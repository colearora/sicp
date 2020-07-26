#lang sicp

;===============================================================================
; Source
;===============================================================================

; Interval data type.
; Supports interval arithmetic.

; Constructors:

; make-interval creates a new interval x from numerical bounds a and b.
; Maintains the invariant that (lower-bound x) <= (upper-bound x).
(define (make-interval a b)
  (if (<= a b) (cons a b) (cons b a)))

; Selectors:

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; Operations:

(define (print-interval x)
  (display "(")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display ")\n"))

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

; width returns half of the difference between the upper and lower bounds
; of interval x. The width is a measure of the uncertainty of the number
; specified by the interval.
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define (neg? x) (< x 0))
(define (non-neg? x) (>= x 0))

;===============================================================================
; Test
;===============================================================================

(define x (make-interval 2 3))
(define y (make-interval -2 2))

(print-interval x) ; (2, 3)
(print-interval y) ; (-2, 2)

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
(width (add-interval x y)) ; 2.5 - for intervals, width of sum is sum of widths.
(width (sub-interval x y)) ; 2.5 - for intervals, width of difference is sum of widths.
(width (mul-interval x y)) ; 6.0 - no such pattern for multiplication/division.


