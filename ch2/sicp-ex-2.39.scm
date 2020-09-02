#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (reverse1 seq)
  (fold-right (lambda (x y)
                (if (null? y)
                    (cons x y)
                    (append y (list x))))
              nil
              seq))

(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define fold-right accumulate)

;===============================================================================
; Test
;===============================================================================

(define seq (list 1 4 9 16 25))
(reverse1 seq) ; (25 16 9 4 1)
(reverse2 seq) ; (25 16 9 4 1)
