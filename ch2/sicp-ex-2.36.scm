#lang sicp

;===============================================================================
; Source
;===============================================================================

; accumulate-n takes a sequence of sequences of the same length (seqs),
; and returns a sequence whose ith element is the result of accumulating
; with op the ith elements of these sequences.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))

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

(define s (list (list 1  2  3)
                (list 4  5  6)
                (list 7  8  9)
                (list 10 11 12)))
(accumulate-n + 0 s) ; (22 26 30)