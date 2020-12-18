
;; Define a very simple mode for fvm.
;; Add to .emacs file: (load (expand-file-name "~/fju/fvm/fvm.el"))

(defvar fvm-mode-syntax-table nil)
(setq fvm-mode-syntax-table
      (let ((st (make-syntax-table)))
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


(defvar fvm-mode-map nil)

(defvar fvm-compiler-path (expand-file-name "~/fju/bin/fvmc") "Path to fvmc compiler")
(defvar fvm-include-path (expand-file-name "~/fju/fvm/stdlib") "Path to fvm stdlib include directory")

(defvar fvm-output-path nil "Path to write compiled module or nil for default")
(make-variable-buffer-local 'fvm-output-path)
(put 'fvm-output-path 'safe-local-variable 'string-or-null-p)

(defun fvm-compile ()
  (interactive)
  (shell-command (if fvm-output-path
		     (format "%s -o %s -I %s %s" fvm-compiler-path fvm-output-path fvm-include-path (buffer-file-name))
		     (format "%s -I %s %s" fvm-compiler-path fvm-include-path (buffer-file-name)))))

(setq fvm-mode-map (make-sparse-keymap))
(define-key fvm-mode-map (kbd "C-c C-c") 'fvm-compile)

(define-derived-mode fvm-mode fundamental-mode "fvm"
  "major mode for fvm pascal"
  (setq font-lock-defaults '(fvm-highlights))
  (setq-local comment-start "{")
  (setq-local comment-end "}")
  (setq-local fvm-output-path nil))






(provide 'fvm-mode)
