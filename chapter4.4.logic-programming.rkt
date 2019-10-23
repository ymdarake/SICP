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

;;
;The general form of a rule is
;(rule ⟨conclusion⟩ ⟨body⟩)
;where ⟨conclusion⟩ is a pattern and ⟨body⟩ is any query.

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 
                    (?town . ?rest-1))
           (address ?person-2 
                    (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle Ben)))

;As in the case of compound procedures, rules can be used as parts of other rules (as we saw with the lives-near rule above) or even be defined recursively.


; Exercise 4.57
(rule (can-replace ?person-1 ?person-2)
      (and (or (and
                (job ?person-1 ?job)
                (job ?person-2 ?job))
               (and
                (job ?person-1 ?job-1)
                (job ?person-2 ?job-2)
                (can-do-job ?job-2 ?job-1))); can-do-job assumed to be a primitive rule.
           (not (same ?person-1 ?person-2))))

(can-replace (Fect Cy D) ?x)

(and (can-replace ?salary-thief ?under-estimated-poor)
     (salary ?under-estimated-poor ?poor-salary)
     (salary ?salary-thief ?high-salary)
     (lisp-value > ?high-salary ?poor-salary))

; Exercise 4.58
(rule (big-shot ?person)
      (and (job ?person (?section . ?type))
           (supervisor ?person ?boss)
           (job ?boss (?boss-section . ?boss-type))
           (not (same ?section ?boss-section))))

; Exercise 4.59
(meeting ?type (Friday ?time))

(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and
           (job ?person (?section . ?type))
           (meeting ?section ?day-and-time))))

(meeting-time (Hacker Alyssa P) (Wednesday ?time))



(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;; input
(append-to-form (a b) (c d) ?z)
;; result
(append-to-form (a b) (c d) (a b c d))

;; input
(append-to-form (a b) ?y (a b c d))
;; result
(append-to-form (a b) (c d) (a b c d))

;; input
(append-to-form ?x ?y (a b c d))
;; result
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))
(append-to-form (a b c d) () (a b c d))

; Exercise 4.60
; This happens because both of two cases satisfy the condition. Orders of names is not the point to the rule.
; We can remove duplicates by sorting names alphabetically.
(and (lives-near ?person-1 ?person-2)
     (lisp-value person< ?person-1 ?person-2))
(define (person->string a1)
  (if (null? a1)
      ""
      (string-append (symbol->string (car a1)) (person->string (cdr a1)))))
(define (person< person-1 person-2)
  (string<? (person->string person-1) (person->string person-2)))

; Exercise 4.63
(rule (grandson-of ?g ?s)
      (and (son-of ?f ?s)
           (son-of ?g ?f)))

(rule (son-of ?m ?s)
      (or (son ?m ?s)
          (and (wife ?m ?w)
               (son ?w ?s))))




