#lang racket


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

;; 3.1
(define (make-accumulator initial-value)
  (let ((acc initial-value))
    (lambda (added)
      (begin (set! acc (+ acc added))
             acc))))

;; 3.2
(define (make-monitored proc)
  (define call-count 0)
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) call-count)
          (else (begin (set! call-count (+ call-count 1))
                       (proc m)))))
  dispatch)

;; 3.3, 3.4
(define (make-secure-account balance password)
  (define consecutive-auth-failure-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) "Wooooooo~~~~~~~~~!")
  (define (dispatch input-password m)
    (if (not (eq? password input-password))
         (lambda (_) (begin
                       (set! consecutive-auth-failure-count (+ consecutive-auth-failure-count 1))
                       (if (>= consecutive-auth-failure-count 7)
                           (call-the-cops)
                           "Incorrect password")))
        (begin
          (set! consecutive-auth-failure-count 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request: MAKE-ACCOUNT" m))))))
  dispatch)


;; 3.1.2: The benefits of introducing Assignment

;It is tempting to conclude this discussion by saying that,
;by introducing assignment and the technique of hiding state in local variables,
;we are able to structure systems in a more modular fashion
;than if all state had to be manipulated explicitly, by passing additional parameters


;; 3.1.3The Costs of Introducing Assignment
;The trouble here is that substitution is based ultimately on the notion that
;the symbols in our language are essentially names for values.
;But as soon as we introduce set! and the idea that the value of a variable can change,
;a variable can no longer be simply a name. Now a variable somehow refers to a place where a value can be stored,
;and the value stored at this place can change.





