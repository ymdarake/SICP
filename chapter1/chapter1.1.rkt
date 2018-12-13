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

;;;1.7

;; Declarative knowledge ("what is")
;; Imperative knowledge ("how to")

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;;Exercise 1.6
; With applicative order evaluation, both then-clause and else-clause are evaluated
; So the shown definition will get into a infinite loop.

;;Exercise 1.7
; The tolerance of 0.001 is significantly large when computing the square root of a small value.
; For very large values, the machine precision is unable to represent small differences between large numbers.

;;Exercise 1.8
(define (cbrt x)
  (cbrt-iter 1.0 x))
(define (cbrt-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cbrt-iter (improve-cube guess x)
                 x)))
(define (good-enough-cube? guess x)
  (< (abs (- (* guess guess guess) x)) 0.01))
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

