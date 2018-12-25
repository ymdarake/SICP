#lang racket

;;; 1.2 Procedures and the Processes They Generate

;;; 1.2.1 Linear Recursion and Iteration

;; the length of deffered operations grow linearly with n. "hidden" information maintained by the interpreter. "where the process is."
(define (factorial-recur n)
  (if (= n 1)
      1
      (* n (factorial-recur (- n 1)))))

;; iterative process
;; state variables, end test
; tail recursive
(define (factorial-iter n)
  (define (iter acc current)
    (if (> current n)
        acc
        (iter (* acc current) (+ current 1))))
  (iter 1 1))

; exercise 1.10
(define (A x y)
  (cond ((= y 0) 9)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; 1.2.2 Tree recursion
;; the number of steps required by a tree-recursive process will be proportional to the NUMBER OF NODES in the tree,
;; while the space required by will be proportional to the MAXIMUM DEPTH of the tree.

(define (fib n)
  (define (iter a b current)
    (if (= current n)
        b
        (iter (+ a b) a (+ current 1))))
  (iter 1 0 0))

;Excercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

; counting down to 3.
(define (f-iter n)
  (define (iter fn-1 fn-2 fn-3 counter)
    (if (< counter 3)
        fn-1
        (iter
         (+ fn-1 (* 2 fn-2) (* 3 fn-3))
         fn-1
         fn-2
         (- counter 1))))
  (if (< n 3)
      n
      (iter 2 1 0 n)))


;Excercise 1.12
(define (pascals-triangle row col)
  (cond ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascals-triangle (- row 1) (- col 1))
                 (pascals-triangle (- row 1) col)))))

(define (even? n)
  (= (remainder n 2) 0))

;Excercise 1.16
(define (expt b n)
  (define (iter base e a)
    (if (= e 0)
        a
        (if (even? e)
            (iter (* base base) (/ e 2) a)
            (iter base (- e 1) (* a base)))))
  (iter b n 1))

