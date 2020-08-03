#lang sicp

;===============================================================================
; Source
;===============================================================================
  
; Data type for a 2D line segment defined in terms of
; start and end points.
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (let ((x-average ; average of the x-coords of the bounding points of s.
         (average (x-point (start-segment s))
                  (x-point (end-segment s))))
        (y-average ; average of the y-coords of the bounding points of s.
         (average (y-point (start-segment s))
                  (y-point (end-segment s)))))
    (make-point x-average y-average)))
         

; Data type for a 2D point.
(define (make-point x y) (cons x y)) 
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")\n"))

(define (average x y) (/ (+ x y) 2.0))
  
;===============================================================================
; Test
;===============================================================================

(define p (make-point 0 0))
(define q (make-point 3 4))
(define s (make-segment p q))
(print-point (midpoint-segment s)) ; (1.5, 2.0)
