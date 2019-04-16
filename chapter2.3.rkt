#lang racket

;;;;; 2.3 Symbolic Data


;;; 2.3.1 Quatation

;; Treated as Data Objects rather than  as Expressions to be evaluated.

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
; (memq 'apple '(pear (apple orange) banana apple prune)); => '(apple prune)

; Exercise 2.54
(define (equal? x y)
  (cond ((and (pair? x) (pair? y))
         (and (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((or (pair? x) (pair? y))
         #f)
        ((and (null? x) (null? y))
         #t)
        ((or (null? x) (null? y))
         #f)
        (else (eq? x y))))

; Exercise 2.55
;The expression
;(car ''abracadabra)
;is read by the interpreter as
;(car (quote (quote abracadabra)))
;or, more clearly illustrated using the quotation mark in place of the first quote special form, as
;(car '(quote abracadabra))
;The argument to car is the list (quote abracadabra) and its first element is the symbol quote, so that's the value that car returns.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))










