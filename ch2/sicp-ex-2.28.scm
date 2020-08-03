#lang sicp

;===============================================================================
; Source
;===============================================================================

; fringe takes a tree (represented as a list) as input
; and returns a flat list of the leaves in order.
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x)) ; leaf
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

; append returns a list consisting first of the items of
; x, then of the items of y.
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

;===============================================================================
; Test
;===============================================================================

(define x (list (list 1 2) (list 3 4)))

(fringe nil) ; ()
(fringe x) ; (1 2 3 4)
(fringe (cons nil x)) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)