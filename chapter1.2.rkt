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

;Excercise 1.17, 1.18
(define (multi a b)
  (define (double x)
    (* 2 x))
  (define (halve x)
    (/ x 2))
  (define (iter base count acc)
    (if (= 0 count)
        acc
        (if (even? count)
            (iter (double base) (halve count) acc); only to transform to reduce steps.
            (iter base (- count 1) (+ acc base))))); calculate.
  (iter a b 0))

;;1.2.5 GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;Excercise 1.20
; remainder is called in the if form to check if the procedure have to stop.


;Excercise 1.21
(define (divides? a b)
  (= 0 (remainder b a)))
(define (smallest-divisor n)
  (define (iter n test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (iter n (next test-divisor)))))
  (define (next divisor)
    (if (= divisor 2)
        3
        (+ divisor 2)))
  (iter n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;Excercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (display "")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end max-count)
  (define (iter current count)
    (timed-prime-test current)
    (if (= count max-count)
        (display "reached to max-count")
        (if (> current end)
            (display " DONE. ")
            (iter (+ current 2) (+ count (if (prime? current) 1 0))))))
  (iter start 0))

;Excercise 1.23
;The  observed ration of the speed of the two algorithms is not 2, but roughtly 1.5.
;This is mainly due to the NEXT procedure's IF test. The input did halve indeed,
;but we need to do an extra IF test.

;;;;;; An+1 = 2An + 1
(define (an x)
  (if (= 1 x)
      1
      (+ 1 (* 2 (an (- x 1))))))
(define (bn x)
  (define (iter current counter)
    (if (= counter x)
        current
        (iter (+ 1 (* 2 current)) (+ 1 counter))))
  (iter 1 1))