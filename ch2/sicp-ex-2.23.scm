#lang sicp

;===============================================================================
; Source
;===============================================================================

; for-each applies proc to each item in items; return values are discarded.
(define (for-each proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each proc (cdr items)))))

;===============================================================================
; Test
;===============================================================================

(define (print . args)
  (for-each (lambda (x) (display x)) args))

(print 1 " " 2 " " 3 "\n") ; 1 2 3