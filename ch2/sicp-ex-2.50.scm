#lang sicp
(#%require sicp-pict)

;===============================================================================
; Source
;===============================================================================

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;===============================================================================
; Test
;===============================================================================

(paint einstein)
(paint (flip-horiz einstein))
(paint (flip-horiz (flip-horiz einstein))) ; same as (paint einstein)

(paint (rotate180 einstein))
(paint (rotate270 einstein))
