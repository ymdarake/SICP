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
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                                     (deriv (base exp) var))))
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

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

; Exercise 2.57
;(define (augend s)
;  (let ((rest (cddr s)))
;    (cond ((= 0 (length rest)) 0)
;          ((= 1 (length rest)) (car rest))
;          (else (cons '+ rest)))))
;(define (multiplicand p)
;  (let ((rest (cddr p)))
;    (cond ((= 0 (length rest)) 1)
;          ((= 1 (length rest)) (car rest))
;          (else (cons '* rest)))))
;; TO BE SIMPLIFIED AS
(define (binary-expression? exp)
  (null? (cdddr exp)))
(define (second-term exp) (caddr exp))
(define (all-but-first-term exp) (cddr exp))
(define (reduce-expression exp op)
  (if (binary-expression? exp)
      (second-term exp)
      (cons op (all-but-first-term exp))))
(define (augend s) (reduce-expression s '+))
(define (multiplicand p) (reduce-expression p '*))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; Exercise 2.56
; DEFINE exponential AS (** base exponent)
(define (exponentiation? exp) (eq? (car exp) '**))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))
;(deriv '(** 2 3) 'x)
;Output: 0
;(deriv '(** x 3) 'x)
;Output: (* 3 (** x 2))
;(deriv '(** x y) 'x)
;Output: (* y (** x (+ y -1)))

;; Exercise 2.57
;a
; To switch from prefix notation to infix, only the positions of the operator and addend are swapped; the augend remains the same. We simply need to change sum?, addend and make-sum.


;;; 2.3.3 Representing Sets

; union-set, intersection-set, element-of-set?, adjoin-set.
; Sets as Unordered Lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))









