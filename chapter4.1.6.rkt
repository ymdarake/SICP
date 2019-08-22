#lang racket

;; 4.1.6 Internal Definitions
(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))
; Our intention here is that the name odd? in the body of the procedure even? should refer to the procedure odd? that is defined after even?.
; The scope of the name odd? is the entire body of f, not just the portion of the body of f starting at the point where the define for odd? occurs.
; Indeed, when we consider that odd? is itself defined in terms of even?—so that even? and odd? are mutually recursive procedures —
; we see that the only satisfactory interpretation of the two defines is to regard them as if the names even? and odd? were being added to the environment simultaneously. 

;More generally, in block structure, the scope of a local name is the entire procedure body in which the define is evaluated.

#|
(lambda ⟨vars⟩
  (define u ⟨e1⟩)
  (define v ⟨e2⟩)
  ⟨e3⟩)
;would be transformed into
(lambda ⟨vars⟩
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u ⟨e1⟩)
    (set! v ⟨e2⟩)
    ⟨e3⟩))
|#

; Exercise 4.16
; 1.
#|
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((ret (assoc var (frame-pairs (first-frame env)))))
          (if ret
              (cdr ret)
              (env-loop (enclosing-environment env))))))
  (env-loop env))
|#
; 2.
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
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
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (make-begin seq) (cons 'begin seq))

(define (scan-out-defines body)
  (define (name-unassigned defines)
    (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines))
  (define (set-values defines)
    (map (lambda (x) (list 'set! (definition-variable x) (definition-value x))) defines))
  (define (defines->let exprs defines not-defines)
    (cond ((null? exprs) (if (null? defines); TODO: reduce 'if' checks to 1
                             body
                             (list (list 'let
                                         (name-unassigned defines)
                                         (make-begin (append (set-values defines) (reverse not-defines)))))))
           ((definition? (car exprs)) (defines->let (cdr exprs) (cons (car exprs) defines) not-defines))
           (else (defines->let (cdr exprs) defines (cons (car exprs) not-defines)))))
    (defines->let body '() '()))

(define *unassigned* '())
(define example-body '((define u ⟨e1⟩) (define v ⟨e2⟩) ⟨e3⟩))
(scan-out-defines example-body)


;;; 4.2 Lazy Evaluation

;Exercise 4.26
(define (unless? exp)
  (tagged-list? exp 'unless))
(define (unless-predicate exp)
  (cadr exp))
(define (unless-consequent exp)
  (caddr exp))
(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))