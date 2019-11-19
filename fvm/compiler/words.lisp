;;;; Copyright Frank James 2019
;;;; This code is licensed under the MIT license.

(in-package #:fvm)


;; ---------------------- Builtin Words ------------------

;; define intrinsic words
(defword nop (:inline t))
(defword swap ()
  (pop r0)
  (pop r1)
  (push r0)
  (push r1))
(defword drop (:inline t)
  (pop r0))
(defword dup ()
  (ldr r0 sp 1)
  (push r0))
(defword 2dup ()
  (ldr r0 sp 1)
  (push r0)
  (push r0))
(defword dup2 () ;; (a b -- a b a b)
  (ldr r0 sp 1)
  (ldr r1 sp 2)
  (push r0)
  (push r1))
(defword rot () ;; (a b c -- c a b)
  (pop r0)
  (pop r1)
  (pop r2)
  (push r0)
  (push r1)
  (push r2))
(defword over () ;; (a b -- a b a)
  (pop r0)
  (pop r1)
  (push r1)
  (push r0)
  (push r1))
(defword tuck () ;; (a b -- b a b)
  (pop r0)
  (pop r1)
  (push r0)
  (push r1)
  (push r0))
(defword ! ()   ;; (value addr -- )
  (pop r0) ;; addr
  (pop r1) ;; value
  (str r0 r1 0))
(defword @ ()   ;; (addr -- [addr])
  (pop r0) ;; get address
  (ldr r1 r0 0)
  (push r1))
(defword + ()
  (pop r0)
  (pop r1)
  (add r0 r0 r1)
  (push r0))
(defword - ()
  (pop r0)
  (pop r1)
  (sub r0 r1 r0)
  (push r0))
(defword * ()
  (pop r0)
  (pop r1)
  (mul r0 r0 r1)
  (push r0))
(defword / () ;; (num div -- quot)
  (pop r0)   ;; div 
  (pop r1)   ;; num 
  (div r0 r1 r0)
  (push r0))
(defword mod () ;; (x n -- x%n)
  (pop r0) ;; n
  (pop r1) ;; x 
  (mod r0 r1 r0)
  (push r0))
(defword zero (:inline t)
  (ldi r0 0)
  (push r0))
(defword 1+ ()
  (pop r0)
  (add r0 r0 1)
  (push r0))
(defword 1- ()
  (pop r0)
  (add r0 r0 -1)
  (push r0))
(defword 2+ ()
  (pop r0)
  (add r0 r0 2)
  (push r0))
(defword 2- ()
  (pop r0)
  (add r0 r0 -2)
  (push r0))
(defword r> ()
  (rpop r0)
  (push r0))
(defword r< ()
  (pop r0)
  (rpush r0))
(defword r@ ()
  (ldr r0 r6 1)
  (push r0))

;; NOT X ::= X NAND X
;; X AND Y ::= (X NAND Y) NAND (X NAND Y)
;; X OR Y ::= (NOT X) NAND (NOT Y)
;; X NOR Y ::= (X OR Y) NAND (X OR Y)
;; X XOR Y ::= (X NAND (X NAND Y)) NAND (Y NAND (X NAND Y))
;; X XNOR Y ::= (X OR Y) NAND (X NAND Y)
(defword nand ()
  (pop r0)
  (pop r1)
  (nand r0 r0 r1)
  (push r0))
(defword not ()
  (pop r0)
  (nand r0 r0 r0)
  (push r0))
(defword and ()
  (pop r0)
  (pop r1)
  (nand r0 r0 r1)
  (nand r0 r0 r0)
  (push r0))
(defword or ()
  not swap not nand)
(defword nor ()
  or dup nand)
(defword xor ()
  (pop r0)
  (pop r1)
  (nand r2 r0 r1)
  (nand r3 r1 r2)
  (nand r4 r0 r2)
  (nand r0 r3 r4)
  (push r0))
(defword xnor ()
  dup2 nand swap or nand)
(defword halt ()
  (lisp +machine-control-register+)    ;; address of machine control register 
  dup @ #x7fff and       ;; get contents of mcr and clear timer bit
  swap !)                ;; store it 
(defword i (:inline t)   ;; get current loop index 
  (ldr r0 rp 1) ;; r0=[rp+1]
  (push r0))
(defword dumpchr () ;; ( char -- )
  (lisp +console-data-register+) !)
(defword true (:inline t)
  (ldi r0 -1)
  (push r0))
(defword false (:inline t)
  zero)
(defword rshift () ;; (x n -- y)
  0 do 2 / loop)
(defword lshift () ;; (x n -- y)
  0 do 2 * loop)

;; remember that strings are compacted i.e. low 8 bits followed by high 8 bits
(defword dumpstr () ;; (addr -- ) 
  begin
    dup 1+ swap @ ;; get next 2 chars and increment address for next iteration
    dup #x00ff and dup if dumpchr else drop then
    8 rshift 2dup if dumpchr else drop then
  until
  drop)
(defword cr ()
  10 dumpchr)
(defword tos ()
  (push r6))
(defword bos ()
  variable *bottom-of-stack*)
(defword zero! () ;; (addr --)
  (pop r0)
  (sti r0 0))
(defword rand () ;; (-- rand)
  (lisp +random-number-generator+) @)

(let ((glbl-pn (gensym))
      (glbl-done (gensym)))
  (defword = () ;; (x y -- f)
    (pop r0)
    (pop r1)
    (cmp r0 r0 r1)
    (lisp `((br-pn ,glbl-pn)))
    true
    (lisp `((br-pnz ,glbl-done)))
    (lisp glbl-pn)
    false
    (lisp glbl-done)))
(let ((glbl-z (gensym))
      (glbl-done (gensym)))
  (defword != () ;; (x y -- f)
    (pop r0)
    (pop r1)
    (cmp r0 r0 r1)
    (lisp `((br-z ,glbl-z)))
    true
    (lisp `((br-pnz ,glbl-done)))
    (lisp glbl-z)
    false
    (lisp glbl-done)))

(let ((glbl-nz (gensym))
      (glbl-done (gensym)))
  (defword > () ;; (x y -- f)
    (pop r0)
    (pop r1)
    (cmp r0 r1 r0)
    (lisp `((br-nz ,glbl-nz)))
    true
    (lisp `((br-pnz ,glbl-done)))
    (lisp glbl-nz)
    false
    (lisp glbl-done)))
(let ((glbl-n (gensym))
      (glbl-done (gensym)))
  (defword >= () ;; (x y -- f)
    (pop r0)  ;; y 
    (pop r1)  ;; x 
    (cmp r0 r1 r0)
    (lisp `((br-n ,glbl-n)))
    true
    (lisp `((br-pnz ,glbl-done)))
    (lisp glbl-n)
    false
    (lisp glbl-done)))
(let ((glbl-pz (gensym))
      (glbl-done (gensym)))
  (defword < () ;; (x y -- f)
    (pop r0)
    (pop r1)
    (cmp r0 r1 r0)
    (lisp `((br-pz ,glbl-pz)))
    true
    (lisp `((br-pnz ,glbl-done)))
    (lisp glbl-pz)
    false
    (lisp glbl-done)))
(let ((glbl-p (gensym))
      (glbl-done (gensym)))
  (defword <= () ;; (x y -- f)
    (pop r0)
    (pop r1)
    (cmp r0 r1 r0)
    (lisp `((br-p ,glbl-p)))
    true
    (lisp `((br-pnz ,glbl-done)))
    (lisp glbl-p)
    false
    (lisp glbl-done)))

(let ((glbl-pn (gensym))
      (glbl-done (gensym)))
  (defword zero? () ;; (x -- f)
    (pop r0)
    (lisp `((br-pn ,glbl-pn)))
    true
    (lisp `((br-pnz ,glbl-done)))
    (lisp glbl-pn)
    false
    (lisp glbl-done)))

;; only to be used from within a do ... loop
(defword break (:inline t)
  (ldr r0 rp 3) ;; get end label address
  ;; pop index and max then jump
  (rpop r1)
  (rpop r2)
  (jmp r0))

(defword dumphexchr (:inline t) ;; (x -- )
  #x000f and ;; mask off low nibble
  dup 10 >= ;; test for >= 10
  if -10 + #\A else #\0 then
  + dumpchr)

(defword dumphex () ;; (x -- )
  4 0 do
    dup 
    12 rshift dumphexchr   ;; dup and print the high 4 bits 
    4 lshift               ;; shift it left by 4 bits 
  loop
  drop)

(defword tick-count () ;; ( -- x)
  #xfe03 @)

(defword time () ;; ( -- high low)
  (br-pnz 1)
  (.blkw #xfe04)
  (ld r0 -2)
  (ldr r1 r0 0)
  (ldr r2 r0 1)
  (push r2)
  (push r1))

(defword read-input () ;; (addr count -- msglen)
  (pop r1) ;; r1=count
  (pop r0) ;; r0=addr
  (ldi r2 0)
  (br-pnz 1)
  (.blkw #xfe06)
  (ld r3 -2)
  (str r3 r2 0) ;; write to device register, r0 receives msglen 
  (push r0))

(defword reset-input () ;; ( -- )
  1 #xfe06 !)

(defword write-output () ;; (addr count -- )
  (pop r1)
  (pop r0)
  (ldi r2 0)
  (br-pnz 1)
  (.blkw #xfe07) ;; address of output data register 
  (ld r3 -2)
  (str r3 r2 0)) ;; write 0 to #xfe07

;; ------------------ Interrupts --------------------

(defword default-isr ()
  halt)

(defword privilege-exception-isr ()
  "PrivilegeException" dumpstr cr
  halt)
(defisr privilege-exception-isr #x00)

(defword illegal-opcode-isr ()
  "IllegalOpcode" dumpstr cr)
(defisr illegal-opcode-isr #x01)

(defword divide-by-zero-isr ()
  "DivideByZero" dumpstr cr
  halt)
(defisr divide-by-zero-isr #x02)
