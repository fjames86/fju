
(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '("babel" "nibbles")))

(defpackage #:fvm
  (:use #:cl))

(in-package #:fvm)

(defun label-offset (label ltab offset max)
  (if (integerp label)
      label
      (let ((val (cdr (assoc label ltab))))
	(unless val
	  (error "Unknown label ~S" label))
	(let ((ret (- val offset)))
	  (when (> (abs ret) max)
	    (error "Offset ~S out of range ~S" ret max))
	  ret))))
	
(defun first-pass (instructions)
  "on the first pass we just compute the label address table"
  (do ((instrs instructions (cdr instrs))
       (offset 0)
       (ltab nil))
      ((null instrs) ltab)
    (let ((inst (car instrs)))
      (cond
	((symbolp inst)
	 ;; a symbol just adds a new label
	 (when (assoc inst ltab)
	   (error "Label ~S already defined" inst))
	 (push (cons inst offset) ltab))
	((not (listp inst))
	 (error "Invalid form ~S" inst))
	(t
	 (let ((cmd (car inst)))
	   (case cmd
	     ((.ORIG .ORIGIN) ;; (:ORIGIN 232) set PC
	      (setf offset (label-offset (cadr inst) ltab 0 #xffff)))
	     (.STRING ;; (:STRING "hello")
	      (incf offset (length (cdr inst))))
	     (.BLKW ;; (.BLKW 123 ...)
	      (incf offset (length (cdr inst))))
	     (otherwise
	      ;; all other commands just increment offset by 1 
	      (incf offset)))))))))


(defun registerp (name)
  (member name '(r0 r1 r2 r3 r4 r5 R6 r7 sp rp)))
(defun register-index (name)
  (ecase name
    (r0 0)
    (r1 1)
    (r2 2)
    (r3 3)
    (r4 4)
    (r5 5)
    ((r6 rp) 6)
    ((r7 sp) 7)))

(defun encode-opcode (inst ltab offset)
  (destructuring-bind (cmd &rest params) inst
    (ecase cmd
      (br
       (destructuring-bind (pcoffset &rest flaglist) params
	 (let ((flags 0))
	   (dolist (flag flaglist)
	     (ecase flag
	       (pos (setf flags (logior flags 1)))
	       (zero (setf flags (logior flags 2)))
	       (neg (setf flags (logior flags 4)))))
	   (logior (ash 0 12)
		   (ash flags 9)
		   (logand (label-offset pcoffset ltab offset #xff) #x1ff)))))
      (br-p
       (destructuring-bind (pcoffset) params
	 (encode-opcode `(br ,pcoffset pos) ltab offset)))
      (br-n
       (destructuring-bind (pcoffset) params
	 (encode-opcode `(br ,pcoffset neg) ltab offset)))
      (br-z
       (destructuring-bind (pcoffset) params
	 (encode-opcode `(br ,pcoffset zero) ltab offset)))
      ((br-pz br-zp)
       (destructuring-bind (pcoffset) params
	 (encode-opcode `(br ,pcoffset pos zero) ltab offset)))
      ((br-nz br-zn)
       (destructuring-bind (pcoffset) params
	 (encode-opcode `(br ,pcoffset neg zero) ltab offset)))      
      ((br-pn br-np)
       (destructuring-bind (pcoffset) params
	 (encode-opcode `(br ,pcoffset pos neg) ltab offset)))
      ((br-pnz br-pzn br-npz br-nzp br-zpn br-znp)
       (destructuring-bind (pcoffset) params
	 (encode-opcode `(br ,pcoffset pos neg zero) ltab offset)))      
      (add
       (destructuring-bind (dr sr1 src) params
	 (logior (ash 1 12)
		 (ash (register-index dr) 9)
		 (ash (register-index sr1) 6)
		 (if (registerp src) 0 #x20)
		 (if (registerp src)
		     (register-index src)
		     (logand (label-offset src ltab 0 #x1f) #x1ff)))))
      (ld
       (destructuring-bind (dr pcoffset) params
	 (logior (ash 2 12)
		 (ash (register-index dr) 9)
		 (logand (label-offset pcoffset ltab offset #xff) #x1ff))))
      (st
       (destructuring-bind (sr pcoffset) params
	 (logior (ash 3 12)
		 (ash (register-index sr) 9)
		 (logand (label-offset pcoffset ltab offset #xff) #x1ff))))
      (call
       (destructuring-bind (dest) params
	 (logior (ash 4 12)
		 (if (registerp dest) 0 #x800)
		 (if (registerp dest)
		     (register-index dest)
		     (logand (label-offset dest ltab 0 #x7ff) #x7ff)))))
      (nand
       (destructuring-bind (dr sr1 src) params
	 (logior (ash 5 12)
		 (ash (register-index dr) 9)
		 (ash (register-index sr1) 6)
		 (if (registerp src) 0 #x20)
		 (if (registerp src)
		     (register-index src)
		     (logand (label-offset src ltab offset #xf) #x1f)))))
      (ldr
       (destructuring-bind (dr baser src) params
	 (logior (ash 6 12)
		 (ash (register-index dr) 9)
		 (ash (register-index baser) 6)
		 (logand (label-offset src ltab offset #xf) #x1f))))
      (str
       (destructuring-bind (baser sr baseoffset) params
	 (logior (ash 7 12)
		 (ash (register-index sr) 9)
		 (ash (register-index baser) 6)
		 (logand (label-offset baseoffset ltab offset #xf) #x1f))))
      (res4
       (destructuring-bind () params
	 (logior (ash 8 12))))
      (push
       (destructuring-bind (reg) params
	 (logior (ash 9 12)
		 (ash (register-index reg) 9))))
      (rpush
       (destructuring-bind (reg) params
	 (logior (ash 9 12)
		 (ash (register-index reg) 9)
		 #x10)))
      (pop
       (destructuring-bind (reg) params
	 (logior (ash 9 12)
		 (ash (register-index reg) 9)
		 #x20)))
      (rpop
       (destructuring-bind (reg) params
	 (logior (ash 9 12)
		 (ash (register-index reg) 9)
		 #x20
		 #x10)))      
      (ldi
       (destructuring-bind (dr pcoffset) params
	 (logior (ash 10 12)
		 (ash (register-index dr) 9)
		 (label-offset pcoffset ltab offset #xff))))
      (sti
       (destructuring-bind (sr pcoffset) params
	 (logior (ash 11 12)
		 (ash (register-index sr) 9)
		 (logand (label-offset pcoffset ltab offset #xff) #x1ff))))
      (jmp
       (destructuring-bind (reg) params
	 (logior (ash 12 12)
		 (ash (register-index reg) 6))))
      (ret
       (destructuring-bind () params
	 (logior (ash 12 12)
		 #x0800)))
      (mul
       (destructuring-bind (dr sr1 sr2) params
	 (logior (ash 13 12)
		 (ash (register-index dr) 9)
		 (ash (register-index sr1) 6)
		 (ash (register-index sr2) 3))))
      (div
       (destructuring-bind (dr sr1 sr2) params
	 (logior (ash 13 12)
		 (ash (register-index dr) 9)
		 (ash (register-index sr1) 6)
		 (ash (register-index sr2) 3)
		 #x1)))
      (mod 
       (destructuring-bind (dr sr1 sr2) params
	 (logior (ash 13 12)
		 (ash (register-index dr) 9)
		 (ash (register-index sr1) 6)
		 (ash (register-index sr2) 3)
		 #x2)))
      (cmp
       (destructuring-bind (dr sr1 sr2) params
	 (logior (ash 13 12)
		 (ash (register-index dr) 9)
		 (ash (register-index sr1) 6)
		 (ash (register-index sr2) 3)
		 #x3)))
      (lea
       (destructuring-bind (dr pcoffset) params
	 (logior (ash 14 12)
		 (ash (register-index dr) 9)
		 (label-offset pcoffset ltab offset #xff))))
      (res3
       (destructuring-bind (trap-name) params
	 (let ((trap-code (label-offset trap-name ltab 0 #xffff)))
	   (logior (ash 15 12) trap-code)))))))

(defun second-pass (instructions ltab)
  "second pass replaces labels with offsets stored in label table. returns list of 
assembled object code."
  (flet ((obj-code (currobj)
	   (list (car currobj)
		 (nreverse (cadr currobj)))))
    (do ((instrs instructions (cdr instrs))
	 (currobj (list 0 nil))
	 (ret nil)
	 (offset 0))
	((null instrs) (cons (obj-code currobj) ret))
      (let ((inst (car instrs)))
	(cond
	  ((symbolp inst)
	   nil)
	  (t
	   (let ((cmd (car inst)))
	     (case cmd
	       ((.ORIG .ORIGIN)
		;; push current object to ret and reset
		(when (cadr currobj)
		  (push (obj-code currobj) ret))
		(setf offset (label-offset (cadr inst) ltab 0 #xffff)
		      currobj (list offset nil)))
	       (.BLKW
		(dolist (x (cdr inst))
		  (push (logand (label-offset x ltab 0 #xffff) #xffff) (cadr currobj))
		  (incf offset)))
	       (.STRING
		(let ((octets (babel:string-to-octets (cadr inst))))
		  (dotimes (i (length octets))
		    (push (aref octets i) (cadr currobj))
		    (incf offset))
		  (push 0 (cadr currobj))
		  (incf offset)))
	       (otherwise
		;; TODO: eliminate a push followed by pop
		(let ((opcode (encode-opcode (car instrs) ltab (1+ offset))))
		  (push (logand opcode #xffff) (cadr currobj))
		  (incf offset)))))))))))


(defun assemble-instructions (instructions)
  (let ((ltab (first-pass instructions)))
    (second-pass instructions ltab)))

(defun save-program (pathspec objs)
  (let ((octets (make-array (* 2 64 1024) :element-type '(unsigned-byte 8))))
    ;; fill out program data from object definitions 
    (dolist (obj objs)
      (destructuring-bind (offset data) obj
	(dotimes (i (length data))
	  (setf (nibbles:ub16ref/le octets (* 2 (+ offset i)))
		(nth i data)))))
    ;; save to file 
    (with-open-file (f pathspec :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))
      (write-sequence octets f)))
  nil)

;; ---------------

(defparameter *words* nil)
(defun wordp (name)
  (assoc name *words*))
(defun word-deps (name)
  (cadr (wordp name)))
(defun word-assembly (name)
  (caddr (wordp name)))

(defun word-dependencies (body)
  (remove-duplicates
   (mapcan (lambda (wrd)
	     (when (and (symbolp wrd) (wordp wrd))
	       (list wrd)))
	   body)))

(defun generate-word-assembly (body)
  (do ((body body (cdr body))
       (asm nil))
      ((null body) (nreverse asm))
    (labels ((expand-word-asm (wrd)
	       (cond
		 ((symbolp wrd)
		  (cond
		    ((wordp wrd)
		     (list `(call ,wrd)))  ;; TODO: allow function inlining
		    ((eq wrd 'if) ;; if word [else word] then
		     (setf body (cdr body))
		     (let ((if-word (pop body))
			   (else-word nil)
			   (else-label (gensym))
			   (then-label (gensym)))
		       (when (eq (car body) 'else)
			 (setf body (cdr body)
			       else-word (car body)
			       body (cdr body)))
		       (unless (eq (car body) 'then) (error "IF expects THEN"))
		       `((pop r0)
			 (add r0 r0 0) ;; test top of stack
			 (br-pn ,else-label)
			 (call ,if-word)
			 (br-pnz ,then-label)
			 ,else-label
			 ,@(when else-word `((call ,else-word)))
			 ,then-label)))
		    ((eq wrd 'do) ;; 10 0 do body-word loop
		     (setf body (cdr body))
		     (let ((start-label (gensym))
			   (end-label (gensym))
			   (body-word (car body)))
		       (setf body (cdr body))
		       (unless (eq (car body) 'loop) (error "DO BODY LOOP expected"))
		       `((pop r0) ;; start index 
			 (pop r1) ;; max counter 
			 (rpush r1) ;; push loop max onto return stack 
			 (rpush r0) ;; push loop counter onto return stack
			 ,start-label
			 (rpop r0) ;; load current loop index
			 (rpop r1)
			 (cmp r2 r0 r1) ;; compare with max counter
			 (br-pz ,end-label)
			 (rpush r1) 
			 (rpush r0) ;; restore loop index and max 
			 (call ,body-word)
			 ;; increment loop index
			 (rpop r0)
			 (add r0 r0 1)
			 (rpush r0) ;; store updated loop index 
			 (br-pnz ,start-label)
			 ,end-label)))
		    (t (list wrd))))   ;; symbol but not a word, assume an assembly label
		 ((integerp wrd)
		  `((br-pnz 1) ;; skip immediate value and load it 
		    (.blkw ,wrd)
		    (ld r0 -2)
		    (push r0)))
		 ((listp wrd)
		  ;; if wrd is a list it is assembly instruction
		  (list wrd))
		 (t (error "Unknown form ~S" wrd)))))
      (let ((wrd (car body)))
	(dolist (x (expand-word-asm wrd))
	  (push x asm))))))


(defun define-word (name options body)
  (declare (ignore options))
  (list name
	(word-dependencies body)
	(generate-word-assembly body)))
			   
(defmacro defword (name options &rest body)
  (declare (ignore options))
  `(push (define-word ',name nil ',body)
	 *words*))
	    

;; define intrinsic words 
(defword swap ()
  (pop r0)
  (pop r1)
  (push r0)
  (push r1))
(defword drop ()
  (pop r0))
(defword dup ()
  (pop r0)
  (push r0)
  (push r0))
(defword dup2 ()
  (pop r0)
  (pop r1)
  (push r1)
  (push r0)
  (push r1)
  (push r0))
(defword rot ()
  (pop r0)
  (pop r1)
  (pop r2)
  (push r0)
  (push r1)
  (push r2))
(defword over ()
  (pop r0)
  (pop r1)
  (push r1)
  (push r0)
  (push r1))
(defword tuck ()
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
  (nand r1 r1 r1) ;; bitwise not
  (add r1 r1 1) ;; 1+ 
  (add r0 r0 r1)
  (push r0))
(defword * ()
  (pop r0)
  (pop r1)
  (mul r0 r0 r1)
  (push r0))
(defword / ()
  (pop r0)
  (pop r1)
  (div r0 r0 r1)
  (push r0))
(defword mod ()
  (pop r0)
  (pop r1)
  (mod r0 r0 r1)
  (push r0))
(defword zero ()
  (nand r0 r0 0)
  (nand r0 r0 r0)
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
  zero #xfffe !)  ;; write 0 zero address fffe 

(defparameter *variables* nil)
(defun define-variable (name size &optional initial-contents)
  (push (list name size initial-contents) *variables*))
(defmacro defvariable (name size &optional initial-contents)
  `(define-variable ',name ,size ,initial-contents))

(defvariable 8words 8 '(1 2 3 4 5 6 7 8))

;; layout:
;; #x0000 - #x07ff  word definition table, 2048 addresses of word definitions
;; #x0800 - #x0fff  unused - interrupt table?
;; #x1000 - #x2fff  return stack
;; #x3000 -         word definitions, global variables 
;; #xfdff           top of data sack
;; #xfe00 - #xffff  device registers

(defun required-words (entry-point)
  (let ((deps (list entry-point)))
    (labels ((get-deps (name)
	       (let ((dps (word-deps name)))
		 (dolist (d dps)
		   (unless (member d deps)
		     (push d deps)
		     (get-deps d))))))
      (get-deps entry-point))
    (nreverse deps)))

(defun generate-assembly (entry-point &key variables)
  (let ((words (required-words entry-point)))
    `((.ORIGIN 0)
      ,@(mapcan (lambda (w)
		  (list w `(.BLKW ,(intern (format nil "~A-DEF" (symbol-name w))))))
		words)
      (.ORIGIN #x3000)
      ,@(mapcan (lambda (word)
		  (append (list (intern (format nil "~A-DEF" (symbol-name word))))
			  (word-assembly word)
			  (list (list 'ret))))
		words)
      ;; put variables immediately after word definitions 
      ,@(mapcan (lambda (var)
		  (destructuring-bind (name size &optional initial-contents) var 
		    (list name ;; label
			  (etypecase size
			    (integer 
			     `(.BLKW ,@(loop :for i :below size :collect (or (nth i initial-contents) 0))))
			    (string
			     `(.STRING ,size))))))
		(mapcan (lambda (varname)
			  (let ((v (assoc varname *variables*)))
			    (when v (list v))))
			variables)))))


			      
(defword test-loop-body ()
  1+)

(defword test ()
  0 10 0 do test-loop-body loop halt)

(defun test ()
  (let ((asm (generate-assembly 'test :variables '(8words))))
    (dolist (x asm)
      (format t ";; ~S~%" x))
    (save-program "test.obj" (assemble-instructions asm))))

