#lang sicp

;===============================================================================
; Source
;===============================================================================

; Set implementation: ordered list.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2 (union-set set1 (cdr set2))))
                 (else
                  (cons x1 (union-set (cdr set1) (cdr set2)))))))))

;===============================================================================
; Test
;===============================================================================

(define set1 '(1 2 3 4 5 6 9))
(define set2 '(1 3 5 7 9))
(define set3 '(8))
(define set4 '())

(element-of-set? 7 set1) ; false
(element-of-set? 7 set2) ; true

(adjoin-set 0 set2) ; (0 1 3 5 7 9)
(adjoin-set 6 set2) ; (1 3 5 6 7 9)
(adjoin-set 11 set2) ; (1 3 5 7 9 11)
(adjoin-set 6 (adjoin-set 6 set2)) ; (1 3 5 6 7 9)

(intersection-set set1 set2) ; (1 3 5 9)
(intersection-set set2 set3) ; ()

(union-set set1 set2) ; (1 2 3 4 5 6 7 9)
(union-set set2 set3) ; (1 3 5 7 8 9)
(union-set set3 set4) ; (8)
(union-set set4 set3) ; (8)

