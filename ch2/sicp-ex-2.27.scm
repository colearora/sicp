#lang sicp

;===============================================================================
; Source
;===============================================================================

; deep-reverse takes a list as argument and returns the list with its elements
; reversed and with all sublists deep-reversed as well.
(define (deep-reverse items)
  (if (pair? items)
      (deep-reverse-iter items nil)
      items))

; deep-reverse-iter is a helper procedure that is parallel recursive
; with deep-reverse.
(define (deep-reverse-iter fwd rev)
  (if (null? fwd)
      rev
      (deep-reverse-iter (cdr fwd)
                         (cons (deep-reverse (car fwd))
                               rev))))

; reverse takes a list as argument and returns the list with its elements
; reversed.
(define (reverse items)
  (define (iter fwd rev)
    (if (null? fwd)
        rev
        (iter (cdr fwd)
              (cons (car fwd) rev))))
  (iter items nil))

;===============================================================================
; Test
;===============================================================================

(define x (list (list 1 2) (list 3 4)))

x                ; ((1 2) (3 4))
(reverse x)      ; ((3 4) (1 2))
(deep-reverse x) ; ((4 3) (2 1))
