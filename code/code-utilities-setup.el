;;; code-utilities-setup.el --- Some general-use utilities for the code modes. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; =============================================================================
;; WHITESPACE: show tabs + trailing whitespace in prog-modes
;; =============================================================================

(define-minor-mode my-minor-prog-mode/show-whitespaces
  "Highlight tabs and trailing whitespace in the current buffer.
Wraps `whitespace-mode' with a buffer-local `whitespace-style' so it
does not interfere with other uses of `whitespace-mode'."
  :lighter " ws"
  (require 'whitespace)
  (if my-minor-prog-mode/show-whitespaces
      (progn
        (setq-local whitespace-style '(face tabs trailing tab-mark))
        (whitespace-mode 1))
    (whitespace-mode -1)
    (kill-local-variable 'whitespace-style)))

(add-hook 'prog-mode-hook #'my-minor-prog-mode/show-whitespaces)

(define-minor-mode my-minor-prog-mode/show-fill-line
  "")

(provide 'code-utilities-setup)
;;; code-utilities-setup.el ends here
