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

(defun crc32 (seq &key crc (start 0) end)
  (declare ((vector (unsigned-byte 8)) seq))  
  (unless crc (setf crc #xffffffff))
  (dotimes (i (- (or end (length seq)) start))
    (setf crc (logand (logxor crc (aref seq (+ start i)))
		      #xffffffff))
    (dotimes (j 8)
      (let ((mask (logand (- (logand crc 1)) #xffffffff)))
	(setf crc (logand (logxor (ash crc -1)
				  (logand #xedb88320 mask))
			  #xffffffff)))))
  (logand (lognot crc) #xffffffff))
	  
(defun obj-data (obj)
  (let ((octets (make-array (* 2 (length obj))
			    :element-type '(unsigned-byte 8))))
    (dotimes (i (length obj))
      (setf (nibbles:ub16ref/le octets (* 2 i)) (nth i obj)))
    octets))
  
(defun %save-program (stream objs)
  ;; write header
  (write-sequence #(#x46 #x56 #x4d #x20) stream)  ;; magic number "FVM " 
  (write-sequence #(0 0 0 0) stream)  ;; version
  (let ((crc #xffffffff))
    (dolist (obj objs)
      (setf crc (crc32 (obj-data (second obj)) :crc crc)))
    (nibbles:write-ub32/le crc stream))
  (write-sequence (make-array (* 29 4) :element-type '(unsigned-byte 8)) stream) ;; spare 
  
  ;; write program data
  (dolist (obj objs)
    (destructuring-bind (offset data) obj
      ;; write object header (just offset and count)
      (let ((tmp (make-array 4 :element-type '(unsigned-byte 8))))
	(setf (nibbles:ub16ref/le tmp 0) offset
	      (nibbles:ub16ref/le tmp 2) (length data))
	(write-sequence tmp stream))
      ;; write object data
      (let ((octets (obj-data data)))
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
  (let ((deps nil))
    (dolist (word body)
      (when (and (symbolp word) (wordp word))
	(if (word-inline-p word)
	    (dolist (w (word-deps word))
	      (pushnew w deps))
	    (pushnew word deps))))
    deps))
(defun word-options (name)
  (cadddr (wordp name)))
(defun word-inline-p (name)
  (getf (word-options name) :inline nil))

;; TODO:
;; need to support nested special forms. 
;; Can we cheat by defining inline words

(defun generate-word-assembly (body)
  (do ((body body)
       (asm nil))
      ((null body) (nreverse asm))
    (labels ((take-words (start-word end-words)
	       (do ((words nil))
		   ((or (member (car body) end-words)
			(null body))
		    (when (null body) (error "Ran out of words before finding end word ~S" end-words))
		    (let ((eword (car body)))
		      (push eword words)
		      (setf body (cdr body))
		      (values (nreverse words) eword)))
		 (cond
		   ((eq (car body) start-word)
		    (setf body (cdr body))
		    (let ((subbody (take-words start-word end-words)))
		      (dolist (w (generate-word-assembly (cons start-word subbody)))
			(push w words))))
		   (t 
		    (push (car body) words)
		    (setf body (cdr body))))))
	     (expand-if-form (if-words else-words)
	       (let ((else-label (gensym))
		     (then-label (gensym)))
		 `((pop r0) ;; sets flags automatically 
		   (br-z ,(if else-words else-label then-label))
		   ,@(generate-word-assembly if-words)
		   ,@(when else-words
			   `((br-pnz ,then-label)
			     ,else-label
			     ,@(generate-word-assembly else-words)))
		   ,then-label)))
	     (expand-word-asm (wrd)
	       (cond
		 ((symbolp wrd)
		  (cond
		    ((wordp wrd)
		     (setf body (cdr body))
		     (if (word-inline-p wrd)
			 (word-assembly wrd)
			 (list `(call ,wrd))))
		    ((eq wrd 'if) ;; if words [else words] then
		     (setf body (cdr body))
		     (let ((if-contents (take-words 'if '(then))))
		       (let ((if-words nil)
			     (else-words nil))
			 ;; extract if-words
			 (do ((ifc if-contents (cdr ifc))
			      (elsep nil))
			     ((eq (car ifc) 'then)
			      (unless (null (cdr ifc)) (error "THEN not final word")))
			   (cond
			     ((eq (car ifc) 'else)
			      (if elsep
				  (error "multiple ELSE")
				  (setf elsep t)))
			     (elsep
			      (push (car ifc) else-words))
			     (t
			      (push (car ifc) if-words))))
			 (setf if-words (nreverse if-words)
			       else-words (nreverse else-words))
			 (expand-if-form if-words else-words))))
		    ((eq wrd 'do) ;; 10 0 do body-word loop
		     (setf body (cdr body))
		     (let ((start-label (gensym))
			   (end-label (gensym))
			   (+loop-p nil))
		       (multiple-value-bind (body-words end-word) (take-words 'do '(loop +loop))
			 (unless (member end-word '(loop +loop)) (error "DO expects LOOP or +LOOP"))
			 (when (eq end-word '+loop) (setf +loop-p t))
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
			 ,@(generate-word-assembly (butlast body-words))
			 ;; increment loop index
			 (rpop r0)
			 ,@(if +loop-p
			       (list '(pop r1)
				     '(add r0 r0 r1))
			       (list '(add r0 r0 1)))
			 (rpush r0) ;; store updated loop index 
			 (br-pnz ,start-label)
			 ,end-label
			 ;; drop end label address from stack 
			 (rpop r0)))))
		    ((eq wrd 'begin)
		     (setf body (cdr body))
		     (let ((begin-words nil)
			   (start-label (gensym)))
		       (setf begin-words (take-words 'begin '(until)))
		       `(,start-label
			 ,@(generate-word-assembly (butlast begin-words))
			 (pop r0) ;; sets flags automatically 
			 (br-pn ,start-label))))
		    ((eq wrd 'variable)
		     ;; get address of a given variable 
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       (setf body (cdr body))
		       `((br-pnz 1)
			 (.blkw ,var-name)
			 (ld r0 -2)
			 (push r0))))
		    ((eq wrd 'variable*)
		     ;; get value of given variable. same as (variable xxx @)
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       (setf body (cdr body))
		       `((br-pnz 1)
			 (.blkw ,var-name)
			 (ld r0 -2)
			 (ldr r1 r0 0)
			 (push r1))))
		    ((eq wrd 'local-variable)
		     ;; allocate local variable
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       (setf body (cdr body))
		       `((br-pnz 1)
			 ,var-name
			 (.blkw 0))))
		    ((eq wrd 'local)
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       (setf body (cdr body))
		       `((lea r0 ,var-name)
			 (push r0))))
		    ((eq wrd 'local@)
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       (setf body (cdr body))
		       `((ld r0 ,var-name)
			 (push r0))))
		    ((eq wrd 'local!)
		     (setf body (cdr body))
		     (let ((var-name (car body)))
		       (setf body (cdr body))
		       `((pop r0)
			 (st r0 ,var-name))))
		    ((eq wrd 'uint32)
		     (setf body (cdr body))
		     (let ((val (car body)))
		       (setf body (cdr body))
		       `((br-pnz 2)
			 (.blkw ,(logand (ash val -16) #xffff) ,(logand val #xffff))
			 (ld r0 -3)
			 (ld r1 -3)
			 (push r0)
			 (push r1))))
		    ((eq wrd 'uint64)
		     (setf body (cdr body))
		     (let ((val (car body)))
		       (setf body (cdr body))
		       `((br-pnz 4)
			 (.blkw ,(logand (ash val -48) #xffff)
				,(logand (ash val -32) #xffff)
				,(logand (ash val -16) #xffff)
				,(logand val #xffff))
			 (ld r0 -5)
			 (ld r1 -5)
			 (ld r2 -5)
			 (ld r3 -5)
			 (push r0)
			 (push r1)
			 (push r2)
			 (push r3))))	    
		    (t (setf body (cdr body))
		       (list wrd))))   ;; symbol but not a word, assume an assembly label
		 ((integerp wrd)
		  (setf body (cdr body))
		  (if (<= (abs wrd) #xff)
		      `((ldi r0 ,wrd) ;; load 8 bit immediates directly 
			(push r0))
		      `((br-pnz 1) ;; skip immediate value and load it 
			(.blkw ,wrd)
			(ld r0 -2)
			(push r0))))
		 ((characterp wrd)
		  (setf body (cdr body))
		  (let ((code (char-code wrd)))
		    `((ldi r0 ,(logand code #x7f))
		      (push r0))))
		 ((stringp wrd)
		  (setf body (cdr body))
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
		  (setf body (cdr body))
		  (list wrd))
		 (t (error "Unknown form ~S" wrd)))))
      (let ((wrdasm (expand-word-asm (car body))))
	(dolist (x wrdasm)
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
  (let ((gensyms (mapcar (lambda (gs)
			   (cons gs (gensym (symbol-name gs))))
			 (getf options :gensyms))))
    (setf (getf options :gensyms) nil)
    `(setf (gethash ',name *words*) 
	   (make-word ',name
		      (list ,@options)
		      (append 
		       ,@(mapcar
			  (lambda (wrd)
			    (cond
			      ((symbolp wrd)
			       `(list ',(or (cdr (assoc wrd gensyms)) wrd)))
			      ((or (integerp wrd) (characterp wrd)) `(list ,wrd))
			      ((stringp wrd) `(list ,wrd)) 
			      ((not (listp wrd)) (error "Unexpected form ~S" wrd))
			      ((eq (car wrd) 'lisp) ;; a way of unquoting
			       (let ((glist (gensym)))
				 `(let ((,glist ,(cadr wrd)))
				    (if (listp ,glist) ,glist (list ,glist)))))
			      (t 
			       `(list ',(mapcar (lambda (w) (or (cdr (assoc w gensyms)) w))
						wrd)))))
			  body))))))

(defparameter *variables* nil)
(defun make-variable (name size &optional initial-contents)
  (list name size initial-contents))
(defun %define-variable (var)
  (dolist (v *variables*)
    (when (eq (car v) (car var))
      (setf (cdr v) (cdr var))
      (return-from %define-variable nil)))
  (push var *variables*))
(defmacro defvariable (name initial-contents &optional size)
  "Define a global variable. 
NAME ::= symbol naming global.
INITIAL-CONTENTS ::= integer, list of integers or string
" 
  `(%define-variable (make-variable ',name ,size ,initial-contents)))


(defmacro define-isr-table (name)
  `(defparameter ,name nil))
(define-isr-table *default-isr-table*)

(defmacro defisr (name (isr-table isr-vector &rest options) &body body)
  (let ((gisr-vector (gensym))
	(gisr (gensym))
	(gfound (gensym)))
    `(let ((,gisr-vector ,isr-vector))
       (defword ,name ,options ,@body)
       (let ((,gfound nil))
	 (dolist (,gisr ,isr-table)
	   (when (= (car ,gisr) ,gisr-vector)
	     (setf (cdr ,gisr) (list ',name)
		   ,gfound t)))
	 (unless ,gfound (push (list ,gisr-vector ',name) ,isr-table))))))
(defun isr-word (isr)
  (second isr))

(defvar *isr* nil)

(defun find-isr (ivec &optional (isr-table *isr*))
  (etypecase ivec
    (integer (find ivec isr-table :key #'first :test #'=))
    (symbol (find ivec isr-table :key #'second :test #'eq))))
  
(defun merge-isr-tables (tables)
  (let ((merged nil))
    (dolist (table tables)
      (dolist (isr table)
	(unless (find-isr (first isr) merged)
	  (push isr merged))))
    merged))

(defun required-words (entry-point)
  (let ((deps (list entry-point)))
    (labels ((get-deps (name)
	       (let ((dps (word-deps name)))
		 (dolist (d dps)
		   (unless (member d deps)
		     (push d deps)
		     (get-deps d))))))
      (get-deps entry-point)
      (dolist (isr *isr*)
	(unless (member (isr-word isr) deps)
	  (push (isr-word isr) deps)
	  (get-deps (isr-word isr)))))
    (nreverse deps)))

(defun generate-isr-table (word-definition-table)
  (do ((ret nil)
       (index 0)
       (isr (sort *isr* #'< :key #'first) (cdr isr)))
      ((null isr) (nreverse ret))
    (destructuring-bind (idx word) (car isr)
      (cond
	((> idx index)
	 (setf index idx)
	 (push `(.ORIGIN ,(+ #x0800 index)) ret))
	(t
	 (setf index (1+ idx))))
      (push `(.BLKW ,(cdr (assoc word word-definition-table))) ret))))

(defun generate-assembly (entry-point &key variables extra-words isr-table)
  (let* ((*isr* (merge-isr-tables (list *default-isr-table* isr-table)))
	 (words (remove-duplicates
		 (append (required-words entry-point)
			 (mapcan #'required-words extra-words))))
	 (word-definition-table (mapcar (lambda (word) (cons word (gensym))) words)))
    `((.ORIGIN 0) ;; word jump table 
      ,@(mapcan (lambda (w)
		  (list w `(.BLKW ,(cdr (assoc w word-definition-table)))))
		words)
      ;; isr table
      (.ORIGIN #x0800)
      ,@(generate-isr-table word-definition-table)
      (.ORIGIN #x3000)
      ,@(mapcan (lambda (word)
		  (append (list (cdr (assoc word word-definition-table))) ;; word definition label
			  (word-assembly word)
			  (list (list (if (or (eq word 'default-isr) (find-isr word isr-table)) 'rti 'ret)))))
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

(defun %compile-program (entry-word &key variables print-assembly extra-words isr-table)
  "Compile a program. Returns a list of program object codes." 
  (let ((asm (generate-assembly entry-word
				:variables variables
				:extra-words extra-words
				:isr-table isr-table))
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
  
(defun save-program (pathspec entry-word &key variables print-assembly extra-words isr-table)
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
				    :print-assembly print-assembly
				    :isr-table isr-table))))

(defun compile-program (entry-word &key variables print-assembly extra-words isr-table)
  "Compile a program with a given starting word.
ENTRY-WORD ::= symbol naming entry point.
VARIABLES ::= list of variables defined with DEFVARIABLE
PRINT-ASSEMBLY ::= if true then prints entire program assembly listing.
EXTRA-WORDS ::= ensure these words are added to the generated image, even if never called by ENTRY-WORD or its dependencies.

Returns compiled bytecode." 
  (flexi-streams:with-output-to-sequence (f)
    (%save-program f
		   (%compile-program entry-word
				     :variables variables
				     :extra-words extra-words
				     :print-assembly print-assembly
				     :isr-table isr-table))))


(defun pprint-assembly (word)
  "Pretty print assembly for WORD."
  (let ((asms (word-assembly word)))
    (dolist (asm asms)
      (cond
	((symbolp asm)
	 (format t ";; ~S : ~%" asm))
	((listp asm)
	 (case (car asm)
	   ((.ORIG .ORIGIN)
	    (format t ";; ORIGIN #x~4,'0X~%" (cadr asm)))
	   (.BLKW
	    (format t ";; .BLKW       ~{~S~}~%" (cdr asm)))
	   (.STRING
	    (format t ";; .STRING     ~S~%" (cadr asm)))
	   (otherwise
	    (format t ";;             ~{~S ~}~%" asm))))
	(t
	 (error "Unexpected form ~S~%" asm))))))

(defun print-program-script (progdata name &key inlog outlog (autounload-p t) (start-p t) (stream *standard-output*))
  (format stream "/fju/fvm/programs/~A/progdata opaque ~{~2,'0X~}~%" name (coerce progdata 'list))
  (let ((flags 0))
    (when autounload-p (setf flags (logior flags 1)))
    (when start-p (setf flags (logior flags 2)))
    (format stream "/fju/fvm/programs/~A/flags ~A~%" name flags))
  (when inlog (format stream "/fju/fvm/programs/~A/inlog u64 ~A~%" name inlog))
  (when outlog (format stream "/fju/fvm/programs/~A/outlog u64 ~A~%" name outlog)))
	  
  
;; -----------------------------------

