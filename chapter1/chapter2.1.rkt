#lang racket

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



