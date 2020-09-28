#lang sicp
(#%require sicp-pict)

;===============================================================================
; Source
;===============================================================================

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

(define v (make-vect 2 -1))
(define w (make-vect 4  3))

(xcor-vect v) ; 2
(ycor-vect v) ; -1
(add-vect v w) ; (6 2)
(sub-vect v w) ; (-2 -4)
