#lang racket

; 3.2.1

; In the environment model of evaluation,
;a procedure is always a pair consisting of some code and a pointer to an environment.


;The environment model of procedure application can be summarized by two rules:

;A procedure object is applied to a set of arguments by constructing a frame,
;binding the formal parameters of the procedure to the arguments of the call,
;and then evaluating the body of the procedure in the context of the new environment constructed.
;The new frame has as its enclosing environment the environment part of the procedure object being applied.

;A procedure is created by evaluating a λ-expression relative to a given environment.
;The resulting procedure object is a pair consisting of the text of the λ-expression and
;a pointer to the environment in which the procedure was created.


;The environment model thus explains the two key properties that make local procedure definitions a useful technique for modularizing programs:

;The names of the local procedures do not interfere with names external to the enclosing procedure,
;because the local procedure names will be bound in the frame that the procedure creates when it is run,
;rather than being bound in the global environment.
;The local procedures can access the arguments of the enclosing procedure,
;simply by using parameter names as free variables.
;This is because the body of the local procedure is evaluated in an environment that is subordinate to the evaluation environment for the enclosing procedure.