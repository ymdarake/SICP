#lang racket

; 3.3

;(define (cons x y)
;  (let ((new (get-new-pair)))
;    (set-car! new x)
;    (set-cdr! new y)
;    new))

; Exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;(define (append! x y)
;  (set-cdr! (last-pair x) y)
;  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; Exercise 3.13
; mystery does "reverse".
;(define (mystery x)
;  (define (loop x y)
;    (if (null? x)
;        y
;        (let ((temp (cdr x)))
;          (set-cdr! x y)
;          (loop temp x))))
;  (loop x '()))


; Exercise 3.16
(define (count-pairs x)
  (display x)
  (newline)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
(define x '(foo))
(define y (cons x x))
(define str3 (cons y y))
(count-pairs str3)
;(define str4 '(foo bar baz))
;(set-cdr! (cddr str4) str4)
;(count-pairs str4)

; Exercise 3.17
(define (count-pairs-3-17 x) 
  (let ((encountered '())) 
    (define (helper x) 
      (if (or (not (pair? x)) (memq x encountered)) 
          0 
          (begin 
            (set! encountered (cons x encountered)) 
            (+ (helper (car x)) 
               (helper (cdr x)) 
               1)))) 
    (helper x))) 

