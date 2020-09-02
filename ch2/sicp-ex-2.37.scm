#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
           (map (lambda (c)
                  (dot-product r c))
                cols))
         m)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

;===============================================================================
; Test
;===============================================================================

(define v (list 1 2 3))
(define w (list 2 3 4))
(dot-product v w) ; 20 = 1*2 + 2*3 + 3*4

(define m (list (list 2 3) (list 1 4)))
(define n (list (list 5 1) (list 2 1)))
(transpose n) ; ((5 2) (1 1))
(matrix-*-vector m (list 5 2)) ; (16 13)
(matrix-*-matrix m n) ; ((16 5) (13 5))
