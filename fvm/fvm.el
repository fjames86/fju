
;; Define a very simple mode for fvm.
;; Add to .emacs file: (load (expand-file-name "~/fju/fvm/fvm.el"))
;; put this on first line:
;; { -*- mode: fvm -*-  }

(defvar fvm-mode-syntax-table nil)
(setq fvm-mode-syntax-table
      (let ((st (make-syntax-table)))
;	(modify-syntax-entry ?# "<" st) ;; how to do both single line comments?
;	(modify-syntax-entry ?\n ">" st)
	(modify-syntax-entry ?{ "<" st)
	(modify-syntax-entry ?} ">" st)
	st))

(setq fvm-highlights
      (let* ((x-keywords '("Program" "program" "procedure" "Procedure"
			   "Const" "const" "Var" "var" "End" "end" "If" "if"
			   "While" "while" "Do" "do" "Then" "then" "Else" "else"
			   "Goto" "GoTo" "goto" "Call" "call" "Syscall" "syscall"
			   "Begin" "begin" "Declare" "declare" "Include" "include"
			   "Return" "return" "break" "Break" "Continue" "continue"))
	     (x-types '("u32" "U32" "int" "Int" "Integer" "opaque" "Opaque" "string" "String"))
	     (x-keywords-regexp (regexp-opt x-keywords 'words))
	     (x-types-regexp (regexp-opt x-types 'words)))
	`((,x-types-regexp . font-lock-type-face)
	  (,x-keywords-regexp . font-lock-keyword-face))))

(define-derived-mode fvm-mode fundamental-mode "fvm"
  "major mode for fvm pascal"
  (setq font-lock-defaults '(fvm-highlights)))

(provide 'fvm-mode)
