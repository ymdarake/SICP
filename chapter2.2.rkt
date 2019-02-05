#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length-recur items)
  (if (null? items)
      0
      (+ 1 (length-recur (cdr items)))))

(define (length-iter items)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (+ acc 1) (cdr rest))))
  (iter 0 items))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; Exercise 2.17
(define (last-pair ls)
  (define (iter previous rest)
    (if (null? rest)
        previous
        (iter (car rest) (cdr rest))))
  (iter '() ls))

; Exercise 2.18
(define (reverse-iter ls)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (cons (car rest) acc) (cdr rest))))
  (iter '() ls))
