#lang sicp

;===============================================================================
; Source
;===============================================================================

; last-pair returns the list that contains only the last
; element of the (assumed non-empty) list items.
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;===============================================================================
; Test
;===============================================================================

(last-pair (list 23 72 149 34)) ; (34)
