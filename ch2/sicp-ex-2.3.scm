#lang sicp

;===============================================================================
; Source
;===============================================================================
  
; make-rect constructs a 2D rectangle from two points that specify
; its top left and bottom right corner locations.
(define (make-rect top-left bottom-right) ; ctor - takes two points
  (cond ((>= (x-point top-left) (x-point bottom-right))
         (error "make-rect: top-left not left of bottom-right"))
        ((<= (y-point top-left) (y-point bottom-right))
         (error "make-rect: top-left not above bottom-right"))
        (else (cons top-left bottom-right)))) ; canonical form

; Selectors for the x- and y-coordinates of rectange r's edges.
(define (x-left-rect r) (x-point (car r)))
(define (x-right-rect r) (x-point (cdr r)))
(define (y-top-rect r) (y-point (car r)))
(define (y-bottom-rect r) (y-point (cdr r)))

; Selectors for the corner points of rectangle r.
(define (top-left-rect r)
  (make-point (x-left-rect r)
              (y-top-rect r)))
(define (top-right-rect r)
  (make-point (x-right-rect r)
              (y-top-rect r)))
(define (bottom-left-rect r)
  (make-point (x-left-rect r)
              (y-bottom-rect r)))
(define (bottom-right-rect r)
  (make-point (x-right-rect r)
              (y-bottom-rect r)))

; width-rect returns with with of rectangle r.
(define (width-rect r)
  (- (x-right-rect r)
     (x-left-rect r)))

; height-rect returns the height of rectangle r.
(define (height-rect r)
  (- (y-top-rect r)
     (y-bottom-rect r)))

; perim-rect returns the perimeter of rectangle r.
(define (perim-rect r)
  (+ (* 2 (width-rect r))
     (* 2 (height-rect r))))

; area-rect returns the area of rectangle r.
(define (area-rect r)
  (* (width-rect r)
     (height-rect r)))
         
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
  
;===============================================================================
; Test
;===============================================================================

(define top-left (make-point 0 5))
(define bottom-right (make-point 4 0))
(define r (make-rect top-left bottom-right))

(width-rect r)  ; 4
(height-rect r) ; 5
(perim-rect r)  ; 18 (5 + 5 + 4 + 4)
(area-rect r)   ; 20 (4 * 5)
