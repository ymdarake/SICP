#lang racket
(require "math.rkt")

;;; Data Abstraction
;;; a methodology that enables us to isolate how a compound data object is use
;;; from the details of how it is constructed from more primitive data objects.

;; constructor and selectors
;; available as procedures.

;; Pairs: list-structured data

;ex. Arithmetic Operations for Rational Numbers
;With wishful thinking,
;we can express add, sub, mul, div, equal rules as procedures:
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;Exercise 2.1
(define (make-rat n d)
  (define (normalize x y)
    (cond ((and (> 0 x) (> 0 y)) (cons (- x) (- y)))
          ((and (< 0 x) (< 0 y)) (cons x y))
          ((and (> 0 x) (< 0 y)) (cons x y))
          (else (cons (- x) (- y)))))
  (let ((normalized (normalize n d)))
    (let ((n (car normalized))
          (d (cdr normalized)))
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
;(print-rat one-half)


;Exercise 2.2
; Geometry
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-point p)
  (car p))
(define (end-point p)
  (cdr p))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment segment)
  (let ((start (start-point segment))
        (end (end-point segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
;(print-point (midpoint-segment (make-segment (make-point 2.0 3.0) (make-point 12.4 94.5))))

;Exercise 2.3
(define (perimeter rectangle)
  (* 2 (+ (length rectangle)
          (width rectangle))))
(define (area rectangle)
  (* (length rectangle)
     (width rectangle)))
(define (length rectangle)
  (car rectangle))
(define (width rectangle)
  (cdr rectangle))

; use exercise 2.2
(define (make-rectangle segment length)
  (cons length
        (sqrt
         (let ((start (start-point segment))
               (end (end-point segment)))
           (+ (square (- (x-point start) (x-point end)))
              (square (- (y-point start) (y-point end))))))))
; we can also make rectangle by defining bottom-left and top-right.

;;; What Is Meant by Data?
;; In general, we can think of data as defined by some collection of selectors and constructors,
;; together with specified conditions that these procedures must fulfill in order to be a valid representation.
; ex) even CONS can be defined in this way:
(define (pair x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (pair-first p) (p 0))
(define (pair-second p) (p 1))

;Exercise 2.4
(define (pair-lambda x y)
  (lambda (m) (m x y))); two arguments procedure
(define (car-lambda z)
  (z (lambda (p q) p))); apply the procedure to two arguments procedure which returns its first argument
(define (cdr-lambda z)
  (z (lambda (p q) q))); same but returns its second argument.

;Exercise 2.5
(define (count-divisions base n)
  (define (iter acc reduced)
    (if (= (remainder reduced base) 0)
        (iter (+ acc 1) (/ reduced base))
        acc))
  (iter 0 n))
(define (car-int product)
  (count-divisions 2 product))
(define (cdr-int product)
  (count-divisions 3 product))
(define (cons-int a b)
  (* (expt 2 a)
     (expt 3 b)))

; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;one = add-1 zero
;(lambda (f) (lambda (x) (f ((zero f) x))))
; (zero f) is an identity function.
(define one
  (lambda (f) (lambda (x) (f x))))
;two = add-1 one
;(lambda (f) (lambda (x) (f ((one f) x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))


; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
 (make-interval (+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
;Exercise 2.10
(define (div-interval x y)
  (let ((up (upper-bound y))
        (low (lower-bound y)))
    (if (>= 0 (* up low))
        (error "Unable to divide. Interval spans zero.")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))
; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
; Exercise 2.9
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))
;;ex. this is evaluated to 6. (= 2 + 4)
(width-interval (add-interval (make-interval 1 5) (make-interval 5 13)))
;;ex. this is to 5. (= 3 + 2)
(width-interval (sub-interval (make-interval 6 12) (make-interval 10 14)))
;;ex. this is to 54. (/= 3 * 2)
(width-interval (mul-interval (make-interval 6 12) (make-interval 10 14)))


; Exercise 2.11
;; split it into negative pair, opposite pair and positive pair.

; Exercise 2.12
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (make-center-percent c p)
  (let ((percent-in-real (/ p 100)))
    (make-interval (* c (- 1 percent-in-real))
                   (* c (+ 1 percent-in-real)))))
(define (percent i)
  (/ (* 100 (- (upper-bound i) (lower-bound i))) (+ (lower-bound i) (upper-bound i))))

; Exercise 2.13
; No scheme, just math.

; Exercise 2.14, 2.15, 2.16
; In the interval arithmetic system we've defined, some of the laws of algebra that we're accustomed to don't apply to certain operations,
; so algebraic expressions that are equivalent in a non-interval arithmetic system are not necessarily equivalent in an interval arithmetic system.
; For example, consider the distributive law. The distributive law states that
; <math> a(b+c) = ab+ac </math>
; but this law does not universally apply in our interval arithmetic system.




