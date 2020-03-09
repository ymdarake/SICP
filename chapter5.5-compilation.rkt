#lang racket
;;;; 5.5 - Compilation

;;; 5.5.1 - Structure of the Compiler

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating 
          exp target linkage))
        ((quoted? exp) 
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable 
          exp target linkage))
        ((assignment? exp)
         (compile-assignment
          exp target linkage))
        ((definition? exp)
         (compile-definition
          exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence 
          (begin-actions exp) target linkage))
        ((cond? exp) 
         (compile 
          (cond->if exp) target linkage))
        ((application? exp)
         (compile-application 
          exp target linkage))
        (else
         (error "Unknown expression type: 
                 COMPILE" 
                exp))))

;; Targets and linkages
; Compile and the code generators that it calls take two arguments in addition to the expression to compile.
; There is a target, which specifies the register in which the compiled code is to return the value of the expression.
; There is also a linkage descriptor, which describes how the code resulting from the compilation of the expression should proceed when it has finished its execution.
; The linkage descriptor can require that the code do one of the following three things:
; - continue at the next instruction in sequence (this is specified by the linkage descriptor next),
; - return from the procedure being compiled (this is specified by the linkage descriptor return), or
; - jump to a named entry point (this is specified by using the designated label as the linkage descriptor).
; - For example, compiling the expression 5 (which is self-evaluating) with a target of the val register and a linkage of next should produce the instruction

(assign val (const 5))
; Compiling the same expression with a linkage of return should produce the instructions

(assign val (const 5))
(goto (reg continue))
; In the first case, execution will continue with the next instruction in the sequence. In the second case, we will return from a procedure call. In both cases, the value of the expression will be placed into the target val register.

;; Instruction sequences and stack usage
; The simplest method for combining instruction sequences is a procedure called append-instruction-sequences.
; It takes as arguments any number of instruction sequences that are to be executed sequentially;
; - it appends them and returns the combined sequence.
; - That is, if ⟨seq1⟩ and ⟨seq2⟩ are sequences of instructions, then evaluating

(append-instruction-sequences ⟨seq1⟩ ⟨seq2⟩)
; produces the sequence

⟨seq₁⟩
⟨seq₂⟩

; Whenever registers might need to be saved, the compiler’s code generators use preserving
; By using preserving to combine instruction sequences the compiler avoids unnecessary stack operations.
; -  if the first sequence modifies the register and the second sequence actually needs the register’s original contents,
; -- then preserving wraps a save and a restore of the register around the first sequence before appending the sequences.
; -- Otherwise, preserving simply returns the appended instruction sequences. 

; Preserving would be inefficient as well as complex, because it would have to analyze each of its instruction sequence arguments
; To avoid such repetitious analysis we will associate with each instruction sequence some information about its register use.
; An instruction sequence will contain three pieces of information:
; - the set of registers that must be initialized before the instructions in the sequence are executed (these registers are said to be needed by the sequence),
; - the set of registers whose values are modified by the instructions in the sequence, and
; - the actual instructions (also called statements) in the sequence.
; The constructor for instruction sequences is thus

(define (make-instruction-sequence 
         needs modifies statements)
  (list needs modifies statements))

; For example, the two-instruction sequence that
; - looks up the value of the variable x in the current environment,
; - assigns the result to val, and then returns, requires registers env and
; - continue to have been initialized, and modifies register val.
; This sequence would therefore be constructed as

(make-instruction-sequence
 '(env continue)
 '(val)
 '((assign val
           (op lookup-variable-value)
           (const x)
           (reg env))
   (goto (reg continue))))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;; 5.5.2 - Compiling Expressions

;; Compiling linkage code
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence 
          '(continue)
          '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

;; Compiling simple expressions


(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign 
       ,target
       (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage 
   linkage
   (make-instruction-sequence 
    '(env)
    (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

; All these assignment instructions modify the target register, and the one that looks up a variable needs the env register.

(define (compile-assignment 
         exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 
                  'val
                  'next)))
    (end-with-linkage 
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition 
         exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp)
                  'val
                  'next)))
    (end-with-linkage
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))


;; Compiling conditional expressions
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) 
               after-if
               linkage)))
      (let ((p-code 
             (compile (if-predicate exp)
                      'val
                      'next))
            (c-code
             (compile (if-consequent exp) 
                      target 
                      consequent-linkage))
            (a-code
             (compile (if-alternative exp)
                      target
                      linkage)))
        (preserving 
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence 
           '(val) 
           '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences 
            t-branch c-code)
           (append-instruction-sequences
            f-branch a-code))
          after-if))))))

;; Compiling sequences
; Each expression of the sequence is compiled—the last expression with the linkage specified for the sequence, and
; the other expressions with linkage next (to execute the rest of the sequence).
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq)
                         target
                         linkage))))

;; Compiling lambda expressions
; Compile-lambda generates the code for constructing the procedure object followed by the code for the procedure body.
; The procedure object will be constructed at run time by combining the current environment
; (the environment at the point of definition) with the entry point to the compiled procedure body (a newly generated label).
(define (compile-lambda exp target linkage)
  (let ((proc-entry 
         (make-label 'entry))
        (after-lambda 
         (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage 
         lambda-linkage
         (make-instruction-sequence 
          '(env)
          (list target)
          `((assign 
             ,target
             (op make-compiled-procedure)
             (label ,proc-entry)
             (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env 
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp)
                       'val
                       'return))))

;;; 5.5.3 - Compiling Combinations

(define (compile-application 
         exp target linkage)
  (let ((proc-code 
         (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand)
                (compile operand 'val 'next))
              (operands exp))))
    (preserving 
     '(env continue)
     proc-code
     (preserving 
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call 
       target
       linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes 
         (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence 
         '() 
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence 
                 '(val)
                 '(argl)
                 '((assign argl
                           (op list)
                           (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving 
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving 
          '(argl)
          (car operand-codes)
          (make-instruction-sequence 
           '(val argl)
           '(argl)
           '((assign argl
                     (op cons)
                     (reg val)
                     (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving 
         '(env)
         code-for-next-arg
         (code-to-get-rest-args 
          (cdr operand-codes))))))

;; Applying procedures
; After evaluating the elements of a combination, the compiled code must apply the procedure in proc to the arguments in argl.
; The code performs essentially the same dispatch as the apply procedure in the metacircular evaluator of 4.1.1 or the apply-dispatch entry point in the explicit-control evaluator of 5.4.1.
; It checks whether the procedure to be applied is a primitive procedure or a compiled procedure. For a primitive procedure, it uses apply-primitive-procedure
; we will see shortly how it handles compiled procedures. The procedure-application code has the following form:



;;; 5.5.4 - Combining Instruction Sequences
; an instruction sequence is represented as a list of the registers needed, the registers modified, and the actual instructions.
(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union 
      (registers-needed seq1)
      (list-difference 
       (registers-needed seq2)
       (registers-modified seq1)))
     (list-union
      (registers-modified seq1)
      (registers-modified seq2))
     (append (statements seq1)
             (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences 
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else
         (cons (car s1)
               (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else 
         (cons (car s1)
               (list-difference (cdr s1)
                                s2)))))

; preserving first creates a sequence that has the required saves followed by the statements of seq1 followed by the required restores.
; This sequence needs the registers being saved and restored in addition to the registers needed by seq1,
; and modifies the registers modified by seq1 except for the ones being saved and restored.
; This augmented sequence and seq2 are then appended in the usual way.
; The following procedure implements this strategy recursively, walking down the list of registers to be preserved:3

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and 
             (needs-register? seq2 first-reg)
             (modifies-register? seq1 
                                 first-reg))
            (preserving 
             (cdr regs)
             (make-instruction-sequence
              (list-union 
               (list first-reg)
               (registers-needed seq1))
              (list-difference
               (registers-modified seq1)
               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving 
             (cdr regs)
             seq1
             seq2)))))

(define (tack-on-instruction-sequence 
         seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

(define (parallel-instruction-sequences 
         seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

