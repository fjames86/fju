
;; Define a very simple mode for fvm.
;; Add to .emacs file: (load (expand-file-name "~/fju/fvm/fvm.el"))
;; put this on first line:
;; { -*- mode: fvm -*-  }

(setq fvm-highlights
      (let* ((x-keywords '("Program" "program" "procedure" "Procedure"
			   "Const" "const" "Var" "var" "End" "end" "If" "if"
			   "While" "while" "Do" "do" "Then" "then" "Else" "else"
			   "Goto" "GoTo" "goto" "Call" "call" "Syscall" "syscall"
			   "Begin" "begin" "Declare" "declare" "Include" "include"
			   "Return" "return"))
	     (x-types '("u32" "U32" "int" "Int" "Integer" "opaque" "Opaque" "string" "String"))
	     (x-keywords-regexp (regexp-opt x-keywords 'words))
	     (x-types-regexp (regexp-opt x-types 'words)))
	`((,x-types-regexp . font-lock-type-face)
	  (,x-keywords-regexp . font-lock-builtin-face))))

(define-derived-mode fvm-mode fundamental-mode "fvm"
  "major mode for fvm pascal"
  (setq font-lock-defaults '(fvm-highlights)))

(provide 'fvm-mode)
