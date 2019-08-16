#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)

(define (tagged-list? exp tag)
  (eq? (car exp) tag))

;; 4.1.3: Evaluator Data Structure

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;Operations on Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

#|
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
|#

; Exercise 4.11
(define (make-frame variables values)
  (cons
   'table
   (map cons variables values)))
(define (frame-pairs frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame
            (cons (cons var val) (frame-pairs frame))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((ret (assoc var (frame-pairs (first-frame env)))))
          (if ret
              (cdr ret)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variables -- SET!" var)
        (let ((ret (assoc var (frame-pairs (first-frame env)))))
          (if ret
              (set-cdr! ret val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (ret (assoc var (frame-pairs frame))))
    (if ret
        (set-cdr! ret val)
        (add-binding-to-frame! var val frame))))

(define f (make-frame '(a b c) '(3 4 5)))