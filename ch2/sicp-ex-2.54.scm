#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) ; both are symbols or nil
         (eq? a b))
        ((or  (not (pair? a)) (not (pair? b))) ; one is not a list
         false)
        (else ; both are lists
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))))

;===============================================================================
; Test
;===============================================================================

; true
(equal? 'a 'a)
(equal? '() '())
(equal? '(a) '(a))
(equal? '(a b c d) '(a b c d))
(equal? '((a b) c (d e)) '((a b) c (d e)))

(display "\n")

; false
(equal? 'a 'b)
(equal? '() 'a)
(equal? '() '(a))
(equal? '(a) '(b))
(equal? '(a a a) '(a a a a))
(equal? '(a b c d) '(x b c d))
(equal? '(a b c d) '(a b c x))