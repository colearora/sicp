#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
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

(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items)
          (accumulate op init (cdr items)))))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      nil
      (cons lo
            (enumerate-interval (+ lo 1) hi))))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (define (iter k)
    (cond ((> (square k) n) n)
          ((divides? k n) k)
          (else (iter (+ k 1)))))
  (iter 2))

(define (divides? x y)
  (= (remainder y x) 0))

(define (square x)
  (* x x))

;===============================================================================
; Test
;===============================================================================

(prime? 7)   ; true
(prime? 13)  ; true
(prime? 103) ; true
(prime? 104) ; false

(enumerate-interval 1 6) ; (1 2 3 4 5 6)
(accumulate cons nil (list 1 2 3)) ; (1 2 3)
(append (list 1 2 3) (list 4 5)) ; (1 2 3 4 5)
(accumulate append nil (list (list 1 2) (list 3 4))) ; (1 2 3 4)
(unique-pairs 5) ; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(prime-sum-pairs 6) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

