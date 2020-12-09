
;; Define a very simple mode for fvm.
;; Add to .emacs file: (load (expand-file-name "~/fju/fvm/fvm.el"))
;; put this on first line:
;; { -*- mode: fvm -*-  }

(setq fvm-highlights
      (let* ((x-keywords '("Program" "Procedure" "Const" "Var" "End" "If"
			   "While" "Do" "Then" "Else" "Goto" "Call" "Syscall"
			   "Begin" "Declare" "Include"))
	     (x-types '("u32" "opaque" "string"))
	     (x-keywords-regexp (regexp-opt x-keywords 'words))
	     (x-types-regexp (regexp-opt x-types 'words)))
	`((,x-types-regexp . font-lock-type-face)
	  (,x-keywords-regexp . font-lock-builtin-face))))

(define-derived-mode fvm-mode fundamental-mode "fvm"
  "major mode for fvm pascal"
  (setq font-lock-defaults '(fvm-highlights)))

(provide 'fvm-mode)
