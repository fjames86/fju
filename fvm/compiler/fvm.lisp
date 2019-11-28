;;;; Copyright Frank James 2019
;;;; This code is licensed under the MIT license.

;;;
;;; This file defines a very naive assembler and forth compiler for the 
;;; fvm virtual machine defined in fvm.c
;;; It is not expected to be particularly clever or efficient.
;;; The main aim is to get something that works and is sufficiently
;;; complete that it can actually be used to write programs for fvm.

;; layout:
;; #x0000 - #x07ff  word definition table, 2048 addresses of word definitions
;; #x0800 - #x0900  interrupt service table 
;; #x0900 - #x0fff  unused
;; #x1000 - #x2fff  return stack
;; #x3000 -         word definitions, global variables 
;; #xfdff           top of data sack
;; #xfe00 - #xffff  device registers


(in-package #:fvm)


(defconstant +device-registers+ #xfe00)
(defconstant +machine-control-register+ #xfe00)
(defconstant +console-data-register+ #xfe01)
(defconstant +random-number-generator+ #xfe02)


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
	      (let ((datalen (ceiling (length (cadr inst)) 2)))
		(incf offset datalen)
		(when (zerop (mod (length (cadr inst)) 2)) (incf offset))))
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
      (rti ;; return from interrupt 
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
       (destructuring-bind (dr val) params
	 (logior (ash 10 12)
		 (ash (register-index dr) 9)
		 (logand (label-offset val ltab offset #xff) #x1ff))))
      (sti
       (destructuring-bind (sr val) params
	 (logior (ash 11 12)
		 (ash (register-index sr) 9)
		 (logand (label-offset val ltab offset #xff) #x1ff))))
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
      ((cmp sub)
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
		 (logand (label-offset pcoffset ltab offset #xff) #x1ff))))
      (res3
       (destructuring-bind () params
	 (logior (ash 15 12)))))))

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
		(let* ((octets (babel:string-to-octets (cadr inst)))
		       (datalen (ceiling (length octets) 2)))
		  (dotimes (i datalen)
		    (let ((x (aref octets (* 2 i))))
		      (when (< (1+ (* 2 i)) (length octets))
			(setf x (logior x (ash (aref octets (1+ (* 2 i))) 8))))
		      (push x (cadr currobj))
		      (incf offset)))
		  (when (zerop (mod (length octets) 2))
		    (push 0 (cadr currobj))
		    (incf offset))))
	       (otherwise
		;; TODO: eliminate redundant instructions e.g.
		;; push/pop followed by pop/push (push x) (pop x) or (pop x) (push x) ???
		;; unconditional branch by 0 offset (br-pnz 0) essentially a NOP?? 
		(let ((opcode (encode-opcode (car instrs) ltab (1+ offset))))
		  (push (logand opcode #xffff) (cadr currobj))
		  (incf offset)))))))))))


(defun assemble-instructions (instructions)
  (let ((ltab (first-pass instructions)))
    (second-pass instructions ltab)))

(defun %save-program (stream objs)
    (dolist (obj objs)
      (destructuring-bind (offset data) obj
	;; write object header (just offset and count)
	(let ((tmp (make-array 4 :element-type '(unsigned-byte 8))))
	  (setf (nibbles:ub16ref/le tmp 0) offset
		(nibbles:ub16ref/le tmp 2) (length data))
	  (write-sequence tmp stream))
	;; write object data 
	(let ((octets (make-array (* 2 (length data))
				  :element-type '(unsigned-byte 8))))
	  (dotimes (i (length data))
	    (setf (nibbles:ub16ref/le octets (* 2 i)) (nth i data)))
	  (write-sequence octets stream)))))


;; ----------------------------------------------------------

(defparameter *words* (make-hash-table))
(defun wordp (name)
  (gethash name *words*))
(defun word-deps (name)
  (cadr (wordp name)))
(defun word-assembly (name)
  (caddr (wordp name)))
(defun word-dependencies (body)
  (remove-duplicates
   (mapcan (lambda (wrd)
	     (when (and (symbolp wrd) (wordp wrd))
	       (if (word-inline-p wrd)
		   (word-deps wrd)
		   (list wrd))))
	   body)))
(defun word-options (name)
  (cadddr (wordp name)))
(defun word-inline-p (name)
  (getf (word-options name) :inline nil))

(defun generate-word-assembly (body)
  (do ((body body (cdr body))
       (asm nil))
      ((null body) (nreverse asm))
    (labels ((take-words (end-words)
	       (let ((words nil))
		 (do ()
		     ((or (member (car body) end-words)
			  (null body)))
		   (push (car body) words)
		   (setf body (cdr body)))
		 (nreverse words)))     
	     (expand-word-asm (wrd)
	       (cond
		 ((symbolp wrd)
		  (cond
		    ((wordp wrd)
		     (if (word-inline-p wrd)
			 (word-assembly wrd)
			 (list `(call ,wrd))))
		    ((eq wrd 'if) ;; if word [else word] then
		     (setf body (cdr body))
		     (let ((if-words nil)
			   (else-words nil)
			   (else-label (gensym))
			   (then-label (gensym)))
		       ;; extract if-words
		       (setf if-words (take-words '(else then)))
		       (when (null body) (error "IF expects THEN"))
		       (when (eq (car body) 'else)
			 (setf body (cdr body)
			       else-words (take-words '(then)))
			 (when (null body) (error "IF ELSE expects THEN")))
		       (unless (eq (car body) 'then) (error "IF expects THEN"))
		       `((pop r0) ;; sets flags automatically 
			 (br-z ,else-label)
			 ,@(generate-word-assembly if-words)
			 (br-pnz ,then-label)
			 ,else-label
			 ,@(generate-word-assembly else-words)
			 ,then-label)))
		    ((eq wrd 'do) ;; 10 0 do body-word loop
		     (setf body (cdr body))
		     (let ((start-label (gensym))
			   (end-label (gensym))
			   (body-words nil)
			   (+loop-p nil))
		       (setf body-words (take-words '(loop +loop)))
		       (unless (member (car body) '(loop +loop)) (error "DO expects LOOP or +LOOP"))
		       (when (eq (car body) '+loop) (setf +loop-p t))
		       `((pop r0) ;; start index 
			 (pop r1) ;; max counter
			 (br-pnz 1)
			 (.blkw ,end-label)
			 (ld r2 -2) ;; push end label address to return stack 
			 (rpush r2)			 
			 (rpush r1) ;; push loop max onto return stack 
			 (rpush r0) ;; push loop counter onto return stack
			 ,start-label
			 (rpop r0) ;; load current loop index
			 (rpop r1)
			 (cmp r2 r0 r1) ;; compare with max counter
			 (br-pz ,end-label)
			 (rpush r1) 
			 (rpush r0) ;; restore loop index and max 
			 ,@(generate-word-assembly body-words)
			 ;; increment loop index
			 (rpop r0)
			 ,@(if +loop-p
			       `((pop r1)
				 (add r0 r0 r1))
			       `((add r0 r0 1)))
			 (rpush r0) ;; store updated loop index 
			 (br-pnz ,start-label)
			 ,end-label
			 ;; drop end label address from stack 
			 (rpop r0))))
		    ((eq wrd 'begin)
		     (setf body (cdr body))
		     (let ((begin-words nil)
			   (start-label (gensym)))
		       (setf begin-words (take-words '(until)))
		       (unless (eq (car body) 'until) (error "BEGIN expects UNTIL"))
		       `(,start-label
			 ,@(generate-word-assembly begin-words)
			 (pop r0) ;; sets flags automatically 
			 (br-pn ,start-label))))
		    ((eq wrd 'variable)
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       `((br-pnz 1)
			 (.blkw ,var-name)
			 (ld r0 -2)
			 (push r0))))
		    #+nil((eq wrd 'variable-value)
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       `((br-pnz 1)
			 (.blkw ,var-name)
			 (ld r0 -2) ;; get variable address
			 (ldr r0 r0 0) ;; get variable value
			 (push r0))))			 
		    (t (list wrd))))   ;; symbol but not a word, assume an assembly label
		 ((integerp wrd)
		  (if (<= (abs wrd) #xff)
		      `((ldi r0 ,wrd) ;; load 8 bit immediates directly 
			(push r0))
		      `((br-pnz 1) ;; skip immediate value and load it 
			(.blkw ,wrd)
			(ld r0 -2)
			(push r0))))
		 ((characterp wrd)
		  (let ((code (char-code wrd)))
		    `((ldi r0 ,(logand code #x7f))
		      (push r0))))
		 ((stringp wrd)
		  (let ((lbl (gensym))
			(strlbl (gensym)))
		    `((br-pnz ,lbl)
		      ,strlbl
		      (.string ,wrd)
		      ,lbl
		      (lea r0 ,strlbl)
		      (push r0))))
		 ((listp wrd)
		  ;; if wrd is a list it is assembly instruction
		  (list wrd))
		 (t (error "Unknown form ~S" wrd)))))
      (let ((wrd (car body)))
	(dolist (x (expand-word-asm wrd))
	  (push x asm))))))


(defun make-word (name options body)
  (list name
	(word-dependencies body)
	(generate-word-assembly body)
	options))
			   
(defmacro defword (name options &rest body)
  "Define word
NAME ::= symbol naming word 
OPTIONS ::= List of optioins, :inline 
BODY ::= word definition. List of words or inline assembly.
" 
  `(let ((word 
	  (make-word ',name (list ,@options)
		     (append 
		      ,@(mapcar
			 (lambda (wrd)
			   (cond
			     ((symbolp wrd) `(list ',wrd))
			     ((or (integerp wrd) (characterp wrd)) `(list ,wrd))
			     ((stringp wrd) `(list ,wrd)) 
			     ((not (listp wrd)) (error "Unexpected form ~S" wrd))
			     ((eq (car wrd) 'lisp) ;; a way of unquoting
			      (let ((glist (gensym)))
				`(let ((,glist ,(cadr wrd)))
				   (if (listp ,glist) ,glist (list ,glist)))))
			     (t 
			      `(list ',wrd))))
			 body)))))
     (setf (gethash ',name *words*) word)
     word))
	    

(defparameter *variables* nil)
(defun make-variable (name size &optional initial-contents)
  (list name size initial-contents))
(defmacro defvariable (name initial-contents &optional size)
  "Define a global variable. 
NAME ::= symbol naming global.
INITIAL-CONTENTS ::= integer, list of integers or string
" 
  `(push (make-variable ',name ,size ,initial-contents) *variables*))

(defparameter *isr* nil)
(defun make-isr (word ivec)
  (list word ivec))
(defmacro defisr (word ivec)
  "Define an interrupt service routine.
WORD ::= symbol naming word designated as handler. This word must NOT be used in regular program operation, only for the purposes of this ISR. 
IVEC ::= integer >= 0 <= 255 specifying the interrupt.
" 
  `(push (make-isr ',word ,ivec) *isr*))

(defun find-isr (ivec)
  (etypecase ivec
    (integer (find ivec *isr* :key #'second :test #'=))
    (symbol (find ivec *isr* :key #'first :test #'eq))))
  


(defun required-words (entry-point)
  (let ((deps (list entry-point)))
    (labels ((get-deps (name)
	       (let ((dps (word-deps name)))
		 (dolist (d dps)
		   (unless (member d deps)
		     (push d deps)
		     (get-deps d))))))
      (get-deps entry-point)
      (dotimes (ivec 256)
	(let ((isr (find-isr ivec)))
	  (cond
	    (isr
	     (unless (member (first isr) deps)
	       (push (first isr) deps)
	       (get-deps (first isr))))
	    (t
	     (unless (member 'default-isr deps)
	       (push 'default-isr deps)
	       (get-deps 'default-isr)))))))
    (nreverse deps)))

(defun generate-assembly (entry-point &key variables extra-words)
  (let ((words (remove-duplicates
		(append (required-words entry-point)
			(mapcan #'required-words extra-words)))))
    `((.ORIGIN 0) ;; word jump table 
      ,@(mapcan (lambda (w)
		  (list w `(.BLKW ,(intern (format nil "~A-DEF" (symbol-name w))))))
		words)
      (.ORIGIN #x800) ;; isr table
      ,@(loop :for ivec :below 256 :collect
	   (let ((isr (find-isr ivec)))
	     `(.BLKW ,(intern (format nil "~A-DEF"
				      (if isr (first isr) 'default-isr))))))
      (.ORIGIN #x3000)
      ,@(mapcan (lambda (word)
		  (append (list (intern (format nil "~A-DEF" (symbol-name word))))
			  (word-assembly word)
			  (list (list (if (or (eq word 'default-isr) (find-isr word)) 'rti 'ret)))))
		words)
      ;; put variables immediately after word definitions 
      ,@(mapcan (lambda (var)
		  (destructuring-bind (name size initial-contents) var 
		    (list name ;; label for variable 
			  (etypecase initial-contents 
			    (integer
			     (if size 
				 `(.BLKW ,@(loop :for i :below size :collect initial-contents))
				 `(.BLKW ,initial-contents)))
			    (string
			     `(.STRING ,initial-contents))))))
		(mapcan (lambda (varname)
			  (let ((v (assoc varname *variables*)))
			    (when v (list v))))
			variables))
      *bottom-of-stack*)))

(defun %compile-program (entry-word &key variables print-assembly extra-words)
  "Compile a program. Returns a list of program object codes." 
  (let ((asm (generate-assembly entry-word
				:variables variables
				:extra-words extra-words))
	(words (remove-duplicates
		(append (required-words entry-word)
			(mapcan #'required-words extra-words)))))
    
    (when print-assembly
      (dolist (x asm)
	(format t ";; ~S~%" x)))

    (when print-assembly
      (format t "~%;; Words:~%")
	(let ((idx 0))
	  (dolist (wrd words)
	    (format t ";; WORD ~D ~A~%" idx wrd)
	    (incf idx))))
    
    (let ((objs (assemble-instructions asm)))
      (when print-assembly
	;; print assembled object sizes 
	(format t "~%;; Objects: ~%")
	(let ((count 0))
	  (dolist (obj objs)
	    (format t ";; 0x~4,'0X - 0x~4,'0X LENGTH ~D~%"
		    (car obj) (+ (car obj) (length (cadr obj))) (length (cadr obj)))
	    (incf count (length (cadr obj))))
	  (format t ";; Total: ~A (~A bytes)~%" count (* 2 count))))
      objs)))
  
(defun save-program (pathspec entry-word &key variables print-assembly extra-words)
  "Compile and save a program.
PATHSPEC ::= where to save the program file.
ENTRY-WORD ::= word designated as entry point.
VARIABLES ::= list of global variables defined with DEFVARIABLE to use.
EXTRA-WORDS ::= list of words which are compiled in even if never called by ENTRY-WORD 
PRINT-ASSEMBLY ::= if true, prints assembly listing and other info.
" 
  (when print-assembly
    (format t ";; ~A~%" (pathname pathspec)))
  (with-open-file (f pathspec :direction :output
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
    (%save-program f
		   (%compile-program entry-word
				    :variables variables
				    :extra-words extra-words
				    :print-assembly print-assembly))))

(defun compile-program (entry-word &key variables print-assembly extra-words)
  (flexi-streams:with-output-to-sequence (f)
    (%save-program f
		   (%compile-program entry-word
				     :variables variables
				     :extra-words extra-words
				     :print-assembly print-assembly))))


