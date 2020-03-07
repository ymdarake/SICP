#lang racket

; 5.4The Explicit-Control Evaluator
; In 5.1 we saw how to transform simple Scheme programs into descriptions of register machines.
; We will now perform this transformation on a more complex program,
; - the metacircular evaluator of 4.1.1–4.1.4,
;   - which shows how the behavior of a Scheme interpreter can be described in terms of the procedures eval and apply.
;   - The explicit-control evaluator that we develop in this section
;     shows how the underlying procedure-calling and argument-passing mechanisms used in the evaluation process
;     can be described in terms of operations on registers and stacks.
; In addition, the explicit-control evaluator can serve as an implementation of a Scheme interpreter,
; written in a language that is very similar to the native machine language of conventional computers.