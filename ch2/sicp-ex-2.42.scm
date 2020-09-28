#lang sicp

;===============================================================================
; Source
;===============================================================================

(define (queens board-size)
  (define (solve k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (placement) (safe? k placement))
         (flatmap
          (lambda (placement)
            (map (lambda (new-row)
                   (place-queen new-row k placement))
                 (enumerate-interval 1 board-size)))
          (solve (- k 1))))))
  (solve board-size))

(define empty-board nil)
(define (safe? k placement)
  (let ((q (list-ref placement (- k 1)))) ; kth queen
    (none-of (lambda (p)
               (and (not (= (col p) k))
                    (check? p q)))
             placement)))
(define (place-queen row col placement)
  (append placement
          (list (make-queen row col))))

(define (make-queen row col)
  (list row col))
(define (row q)
  (car q))
(define (col q)
  (cadr q))
(define (same-row p q)
  (= (row p) (row q)))
(define (same-col p q)
  (= (col p) (col q)))
(define (same-diag p q)
  (= (abs (- (row p) (row q)))
     (abs (- (col p) (col q)))))
(define (check? p q)
  (or (same-row  p q)
      (same-col  p q)
      (same-diag p q)))

(define (list-ref items i)
  (if (= i 0)
      (car items)
      (list-ref (cdr items) (- i 1))))

(define (none-of f items)
  (cond ((null? items) true)
        ((f (car items)) false) ; short-circuit
        (else (none-of f (cdr items)))))
    
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
      (cons lo (enumerate-interval (+ lo 1) hi))))

;===============================================================================
; Test
;===============================================================================

(enumerate-interval 1 4) ; (1 2 3 4)
(list-ref (list (list 2 1) (list 4 2)) 1) ; (4 2)
(safe? 2 (list (list 2 1) (list 4 2))) ; true

(define (test-queen lo hi)
  (cond ((<= lo hi)
         (let ((solns (queens lo)))
           (display lo)
           (display ":\n")
           (display solns)
           (display "\n")
           (display (length solns))
           (display " solution(s)\n\n"))
         (test-queen (+ lo 1) hi))))
(test-queen 1 8)

;(define b0
;  (list empty-board))
;(display "b0 : ")
;(display b0) ; (())
;(display "\n")
;
;(define b1
;  (flatmap
;   (lambda (placement)
;     (map (lambda (new-row)
;            (place-queen new-row 1 placement))
;          (enumerate-interval 1 4)))
;   b0))
;(display "b1 : ")
;(display b1)
;(display "\n")
;
;(define b1f
;  (filter (lambda (placement) (safe? 1 placement))
;          b1))
;(display "b1f: ")
;(display b1f) ; (((1 1)) ((2 1)) ((3 1)) ((4 1)))
;(display "\n")
;
;(define b2
;  (flatmap
;   (lambda (placement)
;     (map (lambda (new-row)
;            (place-queen new-row 2 placement))
;          (enumerate-interval 1 4)))
;   b1f))
;(display "b2 : ")
;(display b2) 
;(display "\n")
;
;(define b2f
;  (filter (lambda (placement) (safe? 2 placement))
;          b2))
;(display "b2f: ")
;(display b2f) ; (((1 1) (3 2)) ((1 1) (4 2)) ((2 1) (4 2)) ((3 1) (1 2)) ((4 1) (1 2)) ((4 1) (2 2)))
;(display "\n")
;
;(define b3
;  (flatmap
;   (lambda (placement)
;     (map (lambda (new-row)
;            (place-queen new-row 3 placement))
;          (enumerate-interval 1 4)))
;   b2f))
;(display "b3 : ")
;(display b3) 
;(display "\n")
;
;(define b3f
;  (filter (lambda (placement) (safe? 3 placement))
;          b3))
;(display "b3f: ")
;(display b3f) ; (((1 1) (4 2) (2 3)) ((2 1) (4 2) (1 3)) ((3 1) (1 2) (4 3)) ((4 1) (1 2) (3 3)))
;(display "\n")
;
;(define b4
;  (flatmap
;   (lambda (placement)
;     (map (lambda (new-row)
;            (place-queen new-row 4 placement))
;          (enumerate-interval 1 4)))
;   b3f))
;(display "b4 : ")
;(display b4) 
;(display "\n")
;
;(define b4f
;  (filter (lambda (placement) (safe? 4 placement))
;          b4))
;(display "b4f: ")
;(display b4f) ; (((2 1) (4 2) (1 3) (3 4)) ((3 1) (1 2) (4 3) (2 4)))
;(display "\n")



