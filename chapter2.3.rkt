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

