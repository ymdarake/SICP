#lang racket

;;;; 5.3 - Storage Allocation and Garbage Collection

;;;5.3.1 - Memory as Vectors

;We presume that there is a special register, free,
; - that always holds a pair pointer containing the next available index, and
; - that we can increment the index part of that pointer to find the next free location.
;For example, the instruction

(assign ⟨reg1⟩
        (op cons)
        (reg ⟨reg₂⟩)
        (reg ⟨reg₃⟩))
; is implemented as the following sequence of vector operations:

(perform (op vector-set!)
         (reg the-cars)
         (reg free)
         (reg ⟨reg₂⟩))
(perform (op vector-set!)
         (reg the-cdrs)
         (reg free)
         (reg ⟨reg₃⟩))
(assign ⟨reg₁⟩ (reg free))
(assign free (op +) (reg free) (const 1))


;The stack can be a list of the saved values, pointed to by a special register the-stack.
; Thus, (save ⟨reg⟩) can be implemented as

(assign the-stack 
        (op cons)
        (reg ⟨reg⟩)
        (reg the-stack))
;Similarly, (restore ⟨reg⟩) can be implemented as

(assign ⟨reg⟩ (op car) (reg the-stack))
(assign the-stack (op cdr) (reg the-stack))
;and (perform (op initialize-stack)) can be implemented as

(assign the-stack (const ()))


;;; 5.3.2 - Maintaining the Illusion of Infinite Memory

;; There are many ways to perform garbage collection.
;; The method we shall examine here is called stop-and-copy.
;; he basic idea is to divide memory into two halves: “working memory” and “free memory.”
; When cons constructs pairs, it allocates these in working memory.
; When working memory is full, we perform garbage collection by locating all the useful pairs in working memory and copying these into consecutive locations in free memory.
; (The useful pairs are located by tracing all the car and cdr pointers,
; starting with the machine registers.)
; We will assume that there is a register called root
; - that contains a pointer to a structure that eventually points at all accessible data.
; We also assume that
; - the-cars and the-cdrs: The current working memory consists of vectors whose base addresses are in registers called
; - new-cars and new-cdrs: The free memory is in registers called
; Garbage collection is triggered when we exhaust the free cells in the current working memory,
; that is, when a cons operation attempts to increment the free pointer beyond the end of the memory vector.
; -
; Figure 5.15
; When the garbage-collection process is complete,
; the root pointer will point into the new memory,
; all objects accessible from the root will have been moved to the new memory,
; and the free pointer will indicate the next place in the new memory where a new pair can be allocated. 
