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

