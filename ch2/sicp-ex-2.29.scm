#lang sicp

;===============================================================================
; Source
;===============================================================================

; make-mobile returns a new mobile from the two branches left and right.
(define (make-mobile left right)
  (list left right))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (cadr m)) ; change to (cdr m) if make-mobile uses cons

; make-branch returns a new branch from a length, which must be a number,
; and a structure, which may be either a number (representing a simple weight)
; or another mobile.
(define (make-branch length structure)
  (list length structure))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cadr b)) ; change to (cdr b) if make-branch uses cons

; total-weight returns the total weight of mobile m, which is the sum of
; the mobile's individual weights.
(define (total-weight m)
  (if (not (pair? m))
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))

; is-balanced returns true if the torques exerted on the left and right sides
; of mobile m are equal and if each of the submobiles hanging off its branches
; is balanced.
(define (is-balanced? m)
  (if (not (pair? m))
      true
      (let ((left-torque (* (branch-length (left-branch m))
                            (total-weight (branch-structure (left-branch m)))))
            (right-torque (* (branch-length (right-branch m))
                             (total-weight (branch-structure (right-branch m))))))
        (and (approx-equal? left-torque right-torque)
             (is-balanced? (branch-structure (left-branch m)))
             (is-balanced? (branch-structure (right-branch m)))))))

(define tolerance 0.0001)
(define (approx-equal? x y)
  (< (abs (- x y)) tolerance))

;===============================================================================
; Test
;===============================================================================

(define m1
  (make-mobile (make-branch 2 2)
               (make-branch 1 4)))
(define m2
  (make-mobile (make-branch 2 3)
               (make-branch 4 5)))
(define m3
  (make-mobile (make-branch 2
                            (make-mobile (make-branch 9 1)
                                         (make-branch 5 2)))
               (make-branch 4 8)))

(total-weight m1) ; 6
(total-weight m2) ; 8
(total-weight m3) ; 11

(is-balanced? m1) ; true
(is-balanced? m2) ; false
(is-balanced? m3) ; false