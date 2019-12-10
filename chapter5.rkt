#lang racket

;;;; Chapter 5.1

;; 2nd form of the GCD machine.
;; We will use this register-machine language throughout this chapter,
;; because we will be more concerned with understanding controllers than with understanding the elements and connections in data paths.
;; We should keep in mind, however, that data-path design is crucial in designing real machines.
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



