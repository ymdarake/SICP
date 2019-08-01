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
