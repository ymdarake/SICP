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
