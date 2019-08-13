#lang racket

;Eval
;Eval takes as arguments an expression and an environment.
;It classifies the expression and directs its evaluation.
;Eval is structured as a case analysis of the syntactic type of the expression to be evaluated.
;In order to keep the procedure general, we express the determination of the type of an expression abstractly,
;making no commitment to any particular representation for the various types of expressions.
;Each type of expression has a predicate that tests for it and an abstract means for selecting its parts.
;This abstract syntax makes it easy to see how we can change the syntax of the language by using the same evaluator,
;but with a different collection of syntax procedures.


;Primitive expressions
;For self-evaluating expressions, such as numbers, eval returns the expression itself.
;Eval must look up variables in the environment to find their values.

;Special forms
;For quoted expressions, eval returns the expression that was quoted.
;An assignment to (or a definition of) a variable must recursively call eval to compute the new value to be associated with the variable.
;The environment must be modified to change (or create) the binding of the variable.
;An if expression requires special processing of its parts, so as to evaluate the consequent if the predicate is true, and otherwise to evaluate the alternative.
;A lambda expression must be transformed into an applicable procedure
;by packaging together the parameters and body specified by the lambda expression with the environment of the evaluation.
;A begin expression requires evaluating its sequence of expressions in the order in which they appear.
;A case analysis (cond) is transformed into a nest of if expressions and then evaluated.

;Combinations
;For a procedure application, eval must recursively evaluate the operator part and the operands of the combination.
;The resulting procedure and arguments are passed to apply, which handles the actual procedure application.



(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp)
                        env))
        ((cond? exp)
         (eval (cond->if exp)
               env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

;For clarity, eval has been implemented as a case analysis using cond.
;The disadvantage of this is that our procedure handles only a few distinguishable types of expressions,
;and no new ones can be defined without editing the definition of eval.
;In most Lisp implementations, dispatching on the type of an expression is done in a data-directed style.
;This allows a user to add new types of expressions that eval can distinguish, without modifying the definition of eval itself.


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;Conditionals
;The interpreter predicate true? translates that value into a value that can be tested by the if in the implementation language
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;Sequences
;Eval-sequence is used by apply to evaluate the sequence of expressions in a procedure body
;and by eval to evaluate the sequence of expressions in a begin expression.
;It takes as arguments a sequence of expressions and an environment,
;and evaluates the expressions in the order in which they occur. The value returned is the value of the final expression.

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; Exercise 4.1
; left-to-right
(define (list-of-values-lr exps env) 
  (if (no-operands? exps) 
      '() 
      (let ((first (eval (first-operand exps) env))) 
        (let ((rest (list-of-values-lr (rest-operands exps) env))) 
          (cons first rest))))) 
; right-to-left
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values-rf (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))


;;;;;; 4.1.2 Representing Expressions

;;;Here is the specification of the syntax of our language:

;The only self-evaluating items are numbers and strings:
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;Variables are represented by symbols:
(define (variable? exp) (symbol? exp))

;Quotations have the form (quote ⟨text-of-quotation⟩): ... 'a => (quote a)
(define (quoted? exp)
  (tagged-list exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;Assignments have the form (set! ⟨var⟩ ⟨value⟩):
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

;Definitions have the form
;(define ⟨var⟩ ⟨value⟩)
;or the form
;(define (⟨var⟩ ⟨param1⟩ ... ⟨paramn⟩) ⟨body⟩)
;The latter form (standard procedure definition) is syntactic sugar for
;(define ⟨var⟩ (lambda (⟨param1⟩ … ⟨paramn⟩) ⟨body⟩))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp);    (cadr  '(define hoge (lambda () ())))
      (caadr exp))); (caadr '(define (hoge foo bar) (<body>)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)    ;formal parameters ... '(foo bar)
                   (cddr exp))))  ;body ... '(<body>)

;Lambda expressions are lists that begin with the symbol lambda:
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;Conditionals begin with if and have a predicate, a consequent, and an (optional) alternative. If the expression has no alternative part, we provide false as the alternative.
;NOTE: The value of an if expression when the predicate is false and there is no alternative is unspecified in Scheme; we have chosen here to make it false.
;    : We will support the use of the variables true and false in expressions to be evaluated by binding them in the global environment. See 4.1.4.
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false)); SEE NOTE above.
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;Begin packages a sequence of expressions into a single expression.
;We include syntax operations on begin expressions to extract the actual sequence from the begin expression,
;as well as selectors that return the first expression and the rest of the expressions in the sequence
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; for use by cond->if (MEMO: TO BE GONE OVER LATER.)
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin sesq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; Derived expressions

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (let ((action (cond-actions first))
                           (predicate (cond-predicate first)))
                       (if (eq? (car action) '=>)
                           (list (cadr action) predicate)
                           (sequence->exp action)))
                     (expand-clauses rest))))))

; Exercise 4.3
(define operation-table make-table) 
(define get (operation-table 'lookup-proc)) 
(define put (operation-table 'insert-proc)) 
  
(put 'op 'quote text-of-quotation) 
(put 'op 'set! eval-assignment) 
(put 'op 'define eval-definition) 
(put 'op 'if eval-if)
(put 'op 'lambda (lambda (x y)
                   (make-procedure (lambda-parameters x) (lambda-body x) y)))
(put 'op 'begin (lambda (x y)
                  (eval-sequence (begin-sequence x) y)))
(put 'op 'cond (lambda (x y)
                 (evaln (cond->if x) y)))
  
(define (evaln expr env)
  (cond ((self-evaluating? expr) expr) 
        ((variable? expr) (lookup-variable-value expr env)) 
        ((get 'op (car expr)) (applyn (get 'op (car expr) expr env))) 
        ((application? expr)  
         (applyn (evaln (operator expr) env)  
                 (list-of-values (operands expr) env))) 
        (else (error "Unknown expression type -- EVAL" expr))))

; Exercise 4.4
(define (and-clauses exp) (cdr exp))
(define (or-clauses exp) (cdr exp))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (empty-exps? seq) (null? seq))
(define (last-exp? seq) (null? (cdr exp)))

(define (eval-and exps env)
  (cond ((empty-exps? exps) 'true)
        (else (let ((first (eval (first-exp exps) env)))
                (cond ((last-exp? exps) first)
                      (first (eval-and (rest-exps exps) env))
                      (else 'false))))))

(define (eval-or exps env)
  (cond ((empty-exps? exps) 'false)
        (else (let ((first (eval (first-exp exps) env)))
                (cond ((last-exp? exps) first)
                      (first 'true)
                      (else (eval-or (rest-exps exps) env)))))))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  (cond ((empty-exps? clauses) 'false)
        ((last-exp? clauses) (first-exp clauses))
        (else (make-if (first-exp clauses)
                       (expand-and-clauses (rest-exps clauses))
                       'false))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (cond ((empty-clauses clauses) 'false)
        ((last-exp? clauses) (first-exp clauses))
        (else (make-if (first-exp clauses)
                       'true
                       (expand-or-clauses (rest-exps clauses))))))

; Exercise 4.5
(define (expand-clauses-ex4.5 clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (let ((action (cond-actions first));;; <- Revised!
                           (predicate (cond-predicate first)))
                       (if (eq? (car action) '=>)
                           (list (cadr action) predicate)
                           (sequence->exp action)))
                     (expand-clauses-ex4.5 rest))))))

; Exercise 4.6
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-variables exp)
  (map car (cadr exp)))
(define (let-body exp)
  (cddr exp))
(define (let-arguments exp)
  (map cadr (cadr exp)))
(define (let->combination exp)
  (list
   (make-lambda (let-variables exp) (let-body exp)
   (let-arguments exp)))



