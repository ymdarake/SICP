#lang racket

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datus)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) 
     (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) 
     (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;Each generic selector is implemented as a procedure that checks the tag of its argument and
; calls the appropriate procedure for handling data of that type. 
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: 
               REAL-PART" z))))


; To implement the complex-number arithmetic operations,
; we can use the same procedures add-complex, sub-complex, mul-complex, and div-complex from 2.4.1,
; because the selectors they call are generic,
; and so will work with either representation. For example, the procedure add-complex is still
(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

