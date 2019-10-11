#lang racket

;;; 4.4 Logic Programming


;; Exercise 4.55
(supervisor ?x (Bitdiddle Ben))

(job ?x (accounting . ?type))

(address ?x (Slumerville . ?where))



;; Here in thi query language,
;In general,
;(and ⟨query1⟩ ⟨query2⟩ ... ⟨queryn⟩)
;is satisfied by all sets of values for the pattern variables that simultaneously satisfy ⟨query1⟩ ... ⟨queryn⟩.
;;
;(or (supervisor ?x (Bitdiddle Ben))
;    (supervisor ?x (Hacker Alyssa P)))
;will find all employees supervised by Ben Bitdiddle or Alyssa P. Hacker
;;
;(and (supervisor ?x (Bitdiddle Ben))
;     (not (job ?x (computer programmer))))
;finds all people supervised by Ben Bitdiddle who are not computer programmers
;;
;When lisp-value is the first element of a pattern,
;it specifies that the next element is a Lisp predicate to be applied to the rest of the (instantiated) elements as arguments.
;For example, to find all people whose salary is greater than $30,000 we could write268
;(and (salary ?person ?amount)
;     (lisp-value > ?amount 30000))

; Exercise 4.56
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))

(and (salary ?person ?amount)
     (salary (Bitdiddle Ben) ?bens-amount)
     (list-value > ?bens-amount ?amount))

(and (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?type))))
