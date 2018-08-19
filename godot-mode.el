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

;; entry function
(defun godot-mode ()
  "Major mode for the Godot Game Engine."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table godot-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(godot-font-lock-keywords))
  (setq major-mode 'godot-mode)
  (setq mode-name "Godot")
  (run-hooks 'godot-mode-hook))

(provide 'godot-mode)

;;; godot-mode.el ends here
