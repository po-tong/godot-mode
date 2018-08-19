;;; godot-mode.el --- Support for the Godot Game Engine

;; Keywords: languages, godot

;; Version: "0.1.0"

;;; Commentary:

;;; Code:

(defgroup godot nil
  "Supprt for the Godot Game Engine."
  :group 'languages
  :prefix "gd-")

(defcustom godot-mode-hook nil
  "Hook run when entering Godot mode."
  :type 'hook
  :tag "godot-mode-hook"
  :group 'godot)

(defvar gd-underscore-word-syntax-p t
  ;; docs string pulled from python mode, it's not implemented yet. More of a TODO
  "This is set later by defcustom, only initial value here.

If underscore chars should be of ‘syntax-class’ `word', not of `symbol'.
Underscores in word-class makes `forward-word'.
Travels the indentifiers.  Default is t.
See also command `toggle-gd-underscore-word-syntax-p'")


;; syntax highlighting
(defconst godot-keywords-raw
  '(
    "case" "const" "do" "elif" "else" "enum" "export" "extends" "for" "if" "match" "pass" "return" "switch" "var" "while" "signal" "func"
    )
  ;; TODO: actually put in all the keywords
  "All keywords in Godot.  Used for font locking.")

(defconst godot-builtins-raw
  '(
    "print" "_ready" "randomize" "queue_free" "emit_signal" "randi" "randf" "_process"
    )
  ;; TODO: actually put in all the keywords
  "All keywords in Godot.  Used for font locking.")

(defconst godot-var-name-raw
  '(
    "Node2D"
    )
  ;; TODO: actually put in all the keywords
  "All keywords in Godot.  Used for font locking.")

;; (defconst godot-function-name-raw
;;   '(
;;     "process_decay"
;;     )
;;   ;; TODO: actually put in all the keywords
;;   "All keywords in Godot.  Used for font locking.")

(defconst godot-constants-raw
  '(
    "false" "true"
    )
  ;; TODO: actually put in all the keywords
  "All keywords in Godot.  Used for font locking.")

(defconst godot-warnings-raw
  '(
    "self"
    )
  ;; TODO: actually put in all the keywords
  "All keywords in Godot.  Used for font locking.")

(defconst godot-types-raw
  '(
    "int" "bool" "null" "Vector2" "PackedScene"
    )
  ;; TODO: actually put in all the keywords
  "All keywords in Godot.  Used for font locking.")



(defun make-regexp-from-list (raw-list)
  "RAW-LIST."
  (concat (regexp-opt raw-list 'words))
  ;; (concat "\\<" (regexp-opt raw-list 'words) "\\>")
  )
(message (rx symbol-start "def" (1+ space) (group (1+ (or word ?_)))))
(defconst godot-font-lock-keywords-1
  (list
   `(,(make-regexp-from-list godot-warnings-raw) . font-lock-warning-face)
   `("\\<[A-Z_]*\\>" . font-lock-constant-face)
   `(,(make-regexp-from-list godot-builtins-raw) . font-lock-builtin-face)
   `(,(make-regexp-from-list godot-keywords-raw) . font-lock-keyword-face)
   `(,(make-regexp-from-list godot-types-raw) . font-lock-type-face)
   `(,(rx symbol-start "func" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
   `(,(rx symbol-start "var" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-variable-name-face))
   )
  "Minimal highlihgting expressions for Godot Mode.")

(defvar godot-font-lock-keywords godot-font-lock-keywords-1
  "Default highlighting expressions for Godot mode.")

(defvar godot-mode-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?# "<" st)
	(modify-syntax-entry ?\n ">" st)
	(if gd-underscore-word-syntax-p
	    (modify-syntax-entry ?\_ "w" st)
	  (modify-syntax-entry ?\_ "_" st))
	st)
  "Give puntuation syntax to ASCII that normally has symbol.

Syntax or has word syntax and isn't a letter.")

;; indentation
;; rules:
;; 1. if we are at the beginning of the buffer, indent to col 0
;; 2. if we are at an "de-indent" line, we deindent relative to previous line
;; 3. lines following "de-indent" line should be the same as the de-indent line
;; 4. lines following a "start" line should be indent 1 more than start line
;; but it's too hard, so I am only gonna implement new line and indent like the builtin editor.
(defun godot-newline-and-indent ()
  "Go to new line and indent appropriately."
  (interactive)
  (message "you pressed something")
  (newline-and-indent)
  (let ((not-indented t) cur-indent)
    (save-excursion
      (while not-indented
	(forward-line -1)
	(if (looking-at "^[ \t]*\\(func\\|if\\|else\\|elif\\|for\\|while\\)")
	    (progn
	      (setq cur-indent (+ (current-indentation) 8))
	      (setq not-indented nil))
	  (if (bobp)
	      (setq not-indented nil))
	  ))
      )
    (if cur-indent
	(indent-line-to cur-indent)
      (indent-line-to 0))
    )
  )

(defun godot-indent-line ()
  "Indent current line as Godot script code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*[func]")
	  (setq cur-indent 0)
	(if (looking-at "^[ \t]*\\(elif\\|else\\)")
	    (progn
	      (save-excursion
		(forward-line -1)
		(setq cur-indent (- (current-indentation) 8)))
	      )
	  (save-excursion
	    (while not-indented
	      (message "how many")
	      (forward-line -1)
	      (if (looking-at "^[ \t]*\\(func\\|if\\)")
		  (progn
		    (message "test1")
		    (setq cur-indent (+ (current-indentation) 8))
		    (setq not-indented nil))
		(if (bobp)
		    (message "test3")
		  (setq not-indented nil)
		  (progn
		    (message "test")
		    (setq cur-indent (current-indentation))
		    (setq not-indented nil)))))))
	)
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))



(defvar godot-mode-map
  (let (( map (make-keymap)))
    (define-key map "\C-j" 'godot-newline-and-indent)
    map)
  "Keymap for Godot major mode.")

;; entry function
(defun godot-mode ()
  "Major mode for the Godot Game Engine."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table godot-mode-syntax-table)
  (use-local-map godot-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(godot-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'godot-indent-line)
  (setq major-mode 'godot-mode)
  (setq mode-name "Godot")
  (run-hooks 'godot-mode-hook))

(provide 'godot-mode)

;;; godot-mode.el ends here
