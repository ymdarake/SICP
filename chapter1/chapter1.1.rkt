#lang racket

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; substitution models
;; Normal order, Applicative order

; multi-case analysis
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

; two-case analysis 
(define (abs-if x)
  (if (< x 0)
      (- x)
      x))


;;; Exercise

;;1.2
(/
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))

;;1.3
(define (sum-of-two-larger-numbers x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> x y) (> z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))))

;;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
; with an interpreter that uses normal-order evaluation, we will get 0
; with an applicative-order, we will get infinite loop. (because evalutations of arguments come first)

