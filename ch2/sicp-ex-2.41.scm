#lang sicp

;===============================================================================
; Source
;===============================================================================

; Returns all ordered triples (i, j, k) such that 1 <= i < j < k <= n
; and i + j + k = s.
(define (three-sum n s)
  (filter (lambda (triple) (= (sum triple) s))
          (unique-triples n)))

(define (sum items)
  (accumulate + 0 items))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval (+ j 1) n)))
                      (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n)))

(define (filter f items)
  (cond ((null? items) nil)
        ((f (car items)) (cons (car items) (filter f (cdr items))))
        (else                              (filter f (cdr items)))))

(define (flatmap f items)
  (accumulate append nil (map f items)))

(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items))
            (map f (cdr items)))))
  
(define (append list1 list2)
  (accumulate cons list2 list1))

(define (accumulate f init items)
  (if (null? items)
      init
      (f (car items)
         (accumulate f init (cdr items)))))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      nil
      (cons lo
            (enumerate-interval (+ lo 1) hi))))

;===============================================================================
; Test
;===============================================================================

(unique-triples 5) ; ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5))
(three-sum 10 20) ; ((1 9 10) (2 8 10) (3 7 10) (3 8 9) (4 6 10) (4 7 9) (5 6 9) (5 7 8))

