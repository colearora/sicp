#lang sicp
(#%require sicp-pict)

;===============================================================================
; Source
;===============================================================================

(define outline
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define diags
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (segments->painter
   (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
         (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
         (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))

;===============================================================================
; Test
;===============================================================================

(paint outline)
(paint diags)
(paint diamond)
