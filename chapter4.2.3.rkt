#lang racket

;; Streams as Lazy Lists

; In 3.5.1, we showed how to implement streams as delayed lists. We introduced special forms delay and cons-stream,
; which allowed us to construct a “promise” to compute the cdr of a stream, without actually fulfilling that promise until later.
; We could use this general technique of introducing special forms whenever we need more control over the evaluation process
;; but this is awkward.
; For one thing, a special form is not a first-class object like a procedure, so we cannot use it together with higher-order procedures.
; Additionally, we were forced to create streams as a new kind of data object similar but not identical to lists,
; and this required us to reimplement many ordinary list operations (map, append, and so on) for use with streams.
;; With lazy evaluation, streams and lists can be identical, so there is no need for special forms or for separate list and stream operations.

; One way to accomplish this is to extend the lazy evaluator to allow for non-strict primitives, and to implement cons as one of these.

(define (cons x y) (lambda (m) (m x y)))
(define (car pair-proc) (pair-proc (lambda (p q) p)))
(define (cdr pair-proc) (pair-proc (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

;(define ones (cons 1 ones))

;(define integers
;  (cons 1 (add-lists ones integers)))

; Exercise 4.32
; car is also delayed.
;(define xs (cons-stream x xs)); cannot be defined due to the eager evaluation of x.
;(define xs (cons x xs)); can be defined thanks to the lazy evaluation of x.

; Exercise 4.33
; implement quoted list as cons newly defined above.
; '(a b) -> (eval (cons 'a (cons 'b '())) the-global-environment)
(define (quote->cons exp)
  (if (pair? exp)
      (list 'cons (quote->cons (car exp)) (quote->cons (cdr exp)))
      (list 'quote exp)))