#lang sicp

;===============================================================================
; Source
;===============================================================================

; count-leaves takes a tree as input and returns the number of leaves.
(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ (length x) y))
              0
              (map enumerate-tree t)))

; enumerate-tree takes a tree as input and returns a flat list of the leaves.
(define (enumerate-tree t)
  (cond ((null? t) nil)
        ((not (pair? t)) (list t))
        (else (append (enumerate-tree (car t))
                      (enumerate-tree (cdr t))))))

(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

; accumulate returns the result of successively applying op pairwise
; to the elements of seq from back to front with the first application
; between the last element of seq and init.
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

;===============================================================================
; Test
;===============================================================================

(define t (list (list 2 (list 1 5 4)) 3 (list 1 (list 4 6) 7)))

t ; ((2 (1 5 4)) 3 (1 (4 6) 7))
(enumerate-tree t) ; (2 1 5 4 3 1 4 6 7)
(map enumerate-tree t) ; ((2 1 5 4) (3) (1 4 6 7))
(count-leaves t) ; 9