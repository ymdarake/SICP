#lang racket

; 5.4 - The Explicit-Control Evaluator
; In 5.1 we saw how to transform simple Scheme programs into descriptions of register machines.
; We will now perform this transformation on a more complex program,
; - the metacircular evaluator of 4.1.1–4.1.4,
;   - which shows how the behavior of a Scheme interpreter can be described in terms of the procedures eval and apply.
;   - The explicit-control evaluator that we develop in this section
;     shows how the underlying procedure-calling and argument-passing mechanisms used in the evaluation process
;     can be described in terms of operations on registers and stacks.
; In addition, the explicit-control evaluator can serve as an implementation of a Scheme interpreter,
; written in a language that is very similar to the native machine language of conventional computers.

; Our Scheme evaluator register machine includes a stack and seven registers:
; - exp, env, val, continue, proc, argl, and unev.
; - Exp is used to hold the expression to be evaluated, and
; - env contains the environment in which the evaluation is to be performed.
; - At the end of an evaluation, val contains the value obtained by evaluating the expression in the designated environment.
; - The continue register is used to implement recursion, as explained in 5.1.4.
;    - (The evaluator needs to call itself recursively, since evaluating an expression requires evaluating its subexpressions.)
; - The registers proc, argl, and unev are used in evaluating combinations.

;;; 5.4.1 - The Core of the Explicit-Control Evaluator

;; When the controller starts at eval-dispatch, it evaluates the expression specified by exp in the environment specified by env.
; When evaluation is complete, the controller will go to the entry point stored in continue, and the val register will hold the value of the expression.
; As with the metacircular eval, the structure of eval-dispatch is a case analysis on the syntactic type of the expression to be evaluated.

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

;; Evaluating simple expressions
ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val
          (op lookup-variable-value)
          (reg exp)
          (reg env))
  (goto (reg continue))
ev-quoted
  (assign val
          (op text-of-quotation)
          (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev
          (op lambda-parameters)
          (reg exp))
  (assign exp 
          (op lambda-body)
          (reg exp))
  (assign val 
          (op make-procedure)
          (reg unev)
          (reg exp)
          (reg env))
  (goto (reg continue))

;; Evaluating procedure applications
ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign
   continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

ev-appl-operand-loop
  (save argl)
  (assign exp
          (op first-operand)
          (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue 
          (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (assign unev
          (op rest-operands)
          (reg unev))
  (goto (label ev-appl-operand-loop))

;; Evaluation of the last argument is handled differently.
; There is no need to save the environment or the list of unevaluated operands before going to eval-dispatch,
; since they will not be required after the last operand is evaluated.
; Thus, we return from the evaluation to a special entry point ev-appl-accum-last-arg,
; which restores the argument list, accumulates the new argument,
; restores the saved procedure, and goes off to perform the application.
ev-appl-last-arg
  (assign continue 
          (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))

ev-appl-accum-last-arg
  (restore argl)
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (restore proc)
  (goto (label apply-dispatch))

; The details of the argument-evaluation loop determine the order in which the interpreter evaluates the operands of a combination (e.g., left to right or right to left—see Exercise 3.8).
; This order is not determined by the metacircular evaluator, which inherits its control structure from the underlying Scheme in which it is implemented.
; Because the first-operand selector (used in ev-appl-operand-loop to extract successive operands from unev) is implemented as car and the rest-operands selector is implemented as cdr,
; the explicit-control evaluator will evaluate the operands of a combination in left-to-right order.

;; Procedure application
; By the time we get to apply-dispatch, the proc register contains the procedure to apply and argl contains the list of evaluated arguments to which it must be applied.
; The saved value of continue (originally passed to eval-dispatch and saved at ev-application),
;  - which tells where to return with the result of the procedure application, is on the stack.
; When the application is complete, the controller transfers to the entry point specified by the saved continue, with the result of the application in val
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev 
          (op procedure-parameters)
          (reg proc))
  (assign env
          (op procedure-environment)
          (reg proc))
  (assign env
          (op extend-environment)
          (reg unev)
          (reg argl)
          (reg env))
  (assign unev
          (op procedure-body)
          (reg proc))
  (goto (label ev-sequence))

; 5.4.2 - Sequence Evaluation and Tail Recursion

; Explicit begin expressions are evaluated by placing the sequence of expressions to be evaluated in unev, saving continue on the stack, and jumping to ev-sequence.

ev-begin
  (assign unev
          (op begin-actions)
          (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue
          (label ev-sequence-continue))
  (goto (label eval-dispatch))

ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev
          (op rest-exps)
          (reg unev))
  (goto (label ev-sequence))

ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;; Tail recursion
; The metacircular implementation of the evaluator in Chapter 4 does not specify whether the evaluator is tail-recursive,
; because that evaluator inherits its mechanism for saving state from the underlying Scheme. 
; With the explicit-control evaluator, however,
; we can trace through the evaluation process to see when procedure calls cause a net accumulation of information on the stack.

; Our evaluator is tail-recursive, because in order to evaluate the final expression of a sequence
; - we transfer directly to eval-dispatch without saving any information on the stack.
; Hence, evaluating the final expression in a sequence—even if it is a procedure call
; - (as in sqrt-iter, where the if expression, which is the last expression in the procedure body, reduces to a call to sqrt-iter)
; - will not cause any information to be accumulated on the stack.
; -- If we did not think to take advantage of the fact that it was unnecessary to save information in this case
; -- we might have implemented eval-sequence by treating all the expressions in a sequence
; -- in the same way—saving the registers, evaluating the expression, returning to restore the registers, and repeating this until all the expressions have been evaluated:
ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue
          (label ev-sequence-continue))
  (goto (label eval-dispatch))

ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))

ev-sequence-end
  (restore continue)
  (goto (reg continue))

; The only difference is that we go through the save-restore cycle for the last expression in a sequence as well as for the others.
; The interpreter will still give the same value for any expression.
; But this change is fatal to the tail-recursive implementation,
; because we must now return after evaluating the final expression in a sequence in order to undo the (useless) register saves.
; These extra saves will accumulate during a nest of procedure calls. 


;; 5.4.3 - Conditionals, Assignments, and Definitions

ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

;; Assignments and definitions

; The code at ev-assignment first evaluates the value part of the expression and then installs the new value in the environment.
; Set-variable-value! is assumed to be available as a machine operation.

ev-assignment
  (assign unev 
          (op assignment-variable)
          (reg exp))
  (save unev)   ; save variable for later
  (assign exp
          (op assignment-value)
          (reg exp))
  (save env)
  (save continue)
  (assign continue
          (label ev-assignment-1))
  ; evaluate the assignment value:
  (goto (label eval-dispatch))

ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op set-variable-value!)
           (reg unev)
           (reg val)
           (reg env))
  (assign val
          (const ok))
  (goto (reg continue))

; Definitions are handled in a similar way:

ev-definition
  (assign unev 
          (op definition-variable)
          (reg exp))
  (save unev)   ; save variable for later
  (assign exp 
          (op definition-value)
          (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  ; evaluate the definition value:
  (goto (label eval-dispatch))

ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op define-variable!)
           (reg unev)
           (reg val)
           (reg env))
  (assign val (const ok))
  (goto (reg continue))


;; 5.4.4 - Running the Evaluator
; With the implementation of the explicit-control evaluator we come to the end of a development,
; - begun in Chapter 1, in which we have explored successively more precise models of the evaluation process.
; - We started with the relatively informal substitution model,
; - then extended this in Chapter 3 to the environment model, which enabled us to deal with state and change.
; - In the metacircular evaluator of Chapter 4, we used Scheme itself as a language for making more explicit the environment structure constructed during evaluation of an expression.
; - Now, with register machines, we have taken a close look at the evaluator’s mechanisms for storage management, argument passing, and control.

read-eval-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input)
           (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))

print-result
  (perform (op announce-output)
           (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))


(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(read-eval-print-loop
     ⟨entire machine controller 
      as given above⟩)))

(define eceval-operations
  (list (list 'self-evaluating? 
              self-evaluating)
        ⟨complete list of operations 
         for eceval machine⟩))

(define the-global-environment
  (setup-environment))

(start eceval)

