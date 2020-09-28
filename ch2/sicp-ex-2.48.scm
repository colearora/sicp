#lang sicp
(#%require sicp-pict)

;===============================================================================
; Source
;===============================================================================

(define (make-segment start end)
  (list start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v w)
  (make-vect
   (+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (sub-vect v w)
  (add-vect v (scale-vect -1 w)))

;===============================================================================
; Test
;===============================================================================

(define u (make-vect 0.25 0.25))
(define v (make-vect 0.5 0.75))
(define s (make-segment u v))
s ; ((0.25 0.25) (0.5 0.75))
(start-segment s) ; (0.25 0.25)
(end-segment s) ; (0.5 0.75)