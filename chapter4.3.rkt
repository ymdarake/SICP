#lang racket

;;; Nondeterministic Computing

; Amb
(define amb (lambda () #t)); to compile this file.

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

#|
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher 
                      miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require
     (not (= (abs (- smith fletcher)) 1)))
    (require 
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
|#
; =>
;((baker 3) (cooper 2) (fletcher 4)
; (miller 5) (smith 1))

; Exercise 4.39
; Conditions with more failure cases come upper.

; Exercise 4.40
(define (multiple-dwelling)
  (let ((backer (amb 1 2 3 4 5)))
       (require (not (= backer 5)))
       (let ((cooper (amb 1 2 3 4 5)))
            (require (not (= cooper 1)))
            (require (distinct? (list backer cooper)))
            (let ((fletcher (amb 1 2 3 4 5)))
                 (require (not (= fletcher 5)))
                 (require (not (= fletcher 1)))
                 (require (not (= (abs (- fletcher cooper)) 1)))
                 (require (distinct? (list backer cooper fletcher)))
                 (let ((miller (amb 1 2 3 4 5)))
                      (require (> miller cooper))
                      (require (distinct? (list backer cooper fletcher miller)))
                      (let ((smith (amb 1 2 3 4 5)))
                           (require (not (= (abs (- smith fletcher)) 1)))
                           (require (distinct? (list backer cooper fletcher miller smith)))
                           (list (list 'backer backer)
                                 (list 'cooper cooper)
                                 (list 'fletcher fletcher)
                                 (list 'miller miller)
                                 (list 'smith smith))))))))

; Exercise 4.41
(define (multiple-dwelling-pred ls)
  (let ((backer (car ls))
        (cooper (cadr ls))
        (fletcher (caddr ls))
        (miller (cadddr ls))
        (smith (cadddr (cdr ls))))
    (and (distinct? (list backer cooper fletcher miller smith))
         (not (= backer 5))
         (not (= cooper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1)))))

(define (distinct? ls)
  (cond ((null? ls) #t)
        ((null? (cdr ls)) #t)
        ((member (car ls) (cdr ls)) #f)
        (else (distinct? (cdr ls)))))

(filter multiple-dwelling-pred (permutations '(1 2 3 4 5)))

; Exercise 4.42

; make 'and' and 'or':
; (define (and? exp) (tagged-list? exp 'and))
; (define (or? exp) (tagged-list? exp 'or))
; and so on...
(define (xor a b)
  (or (and a (not b))
      (and b (not a))))

(define (liar-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;; Parsing natural language

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))
(define *unparsed* '())

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*)
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

;; Observe that a given input may have more than one legal parse.
;; In the sentence “The professor lectures to the student with the cat,”
;; it may be that the professor is lecturing with the cat, or that the student has the cat.
;; Our nondeterministic program finds both possibilities:
;; ... Asking the evaluator to try again yields ...

; Exercise 4.45 - 49 skipped. Implement Amb Evaluator first.


;;; 4.3.3 Implementing the Amb Evaluator
;; Execution procedures and continuations

;the execution procedures for the ordinary evaluator take one argument: the environment of execution.
;In contrast, the execution procedures in the amb evaluator take three arguments: the environment, and two procedures called continuation procedures

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))
; analyze-amb
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

#|
(ambeval ⟨exp⟩
         the-global-environment
         (lambda (value fail) value)
         (lambda () 'failed))
|#

;; Simple expressions
; The execution procedures for the simplest kinds of expressions are essentially the same as those for the ordinary evaluator, except for the need to manage the continuations. 
; The execution procedures simply succeed with the value of the expression, passing along the failure continuation that was passed to them.
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence 
                (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;; Conditionals and sequences
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating
             ;; the predicate to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for
             ;; evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

;;Definitions and assignments

