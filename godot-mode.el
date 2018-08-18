;;; godot-mode.el --- Support for the Godot Game Engine

;; Keywords: languages, godot

;; Version: "0.1.0"

;;; Commentary:

;;; Code:

(defgroup godot-mode nil
  "Supprt for the Godot Game Engine."
  :group 'languages
  :prefix "gd-")

(defcustom godot-mode-hook nil
  "Hook run when entering Godot mode."
  :type 'hook
  :tag "godot-mode-hook"
  :group 'godot-mode)

;; syntax highlighting

;; indentation

;; entry function
(defun godot-mode ()
  "Major mode for the Godot Game Engine."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'godot-mode)
  (setq mode-name "Godot")
  (run-hooks 'godot-mode-hook))

(provide 'godot-mode)

;;; godot-mode.el ends here
