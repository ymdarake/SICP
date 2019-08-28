#lang racket

;;; Nondeterministic Computing

; Amb

; Amb with no choices—the expression (amb)—is an expression with no acceptable values.
; Operationally, we can think of (amb) as an expression that when evaluated causes the computation to “fail”:
; The computation aborts and no value is produced.
(define (require p)
  (if (not p) (amb) #f))

; An-element-of fails if the list is empty. Otherwise it ambiguously returns either the first element of the list or an element chosen from the rest of the list.
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items)
       (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


; This is like the stream procedure integers-starting-from described in 3.5.2, but with an important difference:
; The stream procedure returns an object that represents the sequence of all integers beginning with n, whereas the amb procedure returns a single integer.

; Abstractly, we can imagine that evaluating an amb expression causes time to split into branches,
; where the computation continues on each branch with one of the possible values of the expression.
; We say that amb represents a nondeterministic choice point.

;;;;; if we have a machine that can execute only one process (or a few concurrent processes),
; we must consider the alternatives sequentially.
; One could imagine modifying an evaluator to pick at random a branch to follow whenever it encounters a choice point.
; Random choice, however, can easily lead to failing values. We might try running the evaluator over and over,
;making random choices and hoping to find a non-failing value, but it is better to systematically search all possible execution paths.
;The amb evaluator that we will develop and work with in this section implements a systematic search as follows:

; backtrack
; If a choice results in a failure, then the evaluator automagically backtracks to the most recent choice point and tries the next alternative.
; If it runs out of alternatives at any choice point, the evaluator will back up to the previous choice point and resume from there.
; This process leads to a search strategy known as depth-first search or chronological backtracking.

; If we want to see the value of the next successful execution
#|
(prime-sum-pair '(1 3 5 8) '(20 35 110))
(3 20)
try-again
(3 110)
try-again
(8 35)
try-again
(prime-sum-pair
  (quote (1 3 5 8))
  (quote (20 35 110)))
|#

; Exercise 4.35
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low
       (an-integer-between (+ 1 low) high)))

; Exercise 4.36
; an-integer-starting-from は継続を失敗することが無いために無限に継続するので、バックトラックできないため計算ができない。
(define (a-pythagorean-triple-from n)
  (let ((k (an-integer-starting-from n)))
       (let ((i (an-integer-between n k)))
            (let ((j (an-integer-between i k)))
                 (require (= (+ (* i i) (* j j)) (* k k)))
                 (list i j k)))))

; Exercise 4.37
; Much better, because Ben's procedure doesn't search k in the amb way.
