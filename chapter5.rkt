#lang racket

;;;; Chapter 5.1

;; 2nd form of the GCD machine.
; We will use this register-machine language throughout this chapter,
; because we will be more concerned with understanding controllers than with understanding the elements and connections in data paths.
; We should keep in mind, however, that data-path design is crucial in designing real machines.
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)


;;; 5.1.2 Abstraction in Machine Design
;; The fact that we have swept a lot of complexity under the rug, however,
;; does not mean that a machine design is unrealistic.
;; We can always replace the complex “primitives” by simpler primitive operations.


;;; 5.1.3 Subroutines
; It would be better to replace these two sequences by branches to a single sequence —a gcd subroutine—
; at the end of which we branch back to the correct place in the main instruction sequence. We can accomplish this as follows:
; Before branching to gcd, we place a distinguishing value (such as 0 or 1) into a special register, continue.
; This is a reasonable approach for handling small problems, but it would be awkward if there were many instances of GCD computations in the controller sequence.
; To decide where to continue executing after the gcd subroutine,
; we would need tests in the data paths and branch instructions in the controller for all the places that use gcd.
; A more powerful method for implementing subroutines is to have the continue register hold the label of the entry point
; in the controller sequence at which execution should continue when the subroutine is finished.
gcd
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd))
gcd-done
 (goto (reg continue))
;; Before calling gcd,
;; we assign to continue the label
;; to which gcd should return.
 (assign continue (label after-gcd-1))
 (goto (label gcd))
after-gcd-1
;; Here is the second call to gcd,
;; with a different continuation.
 (assign continue (label after-gcd-2))
 (goto (label gcd))
after-gcd-2

