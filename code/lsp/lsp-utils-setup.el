;;; lsp-utils-setup.el --- Helper functions for lsp setup. -*- lexical-binding: t; -*-

;;; Commentary:
;; This file has some helper functions to make the actual configuration look
;; concise.

;;; Code:

(cl-defun my-diagnostics/flymake-apply-preset (&key (preset 'complement-flycheck))
  "Apply a named flymake configuration PRESET from `my-diagnostics/flymake-presets'."
  (let ((config (alist-get preset my-diagnostics/flymake-presets)))
    (unless config (user-error "Unknown flymake preset: %s" preset))
    (map-let (:indicator-type
              :fringe-indicator-position
              :mode-line-format
              :show-eol
              :suppress-underlines
              :suppress-flymake-proc)
        config
      (setq flymake-indicator-type                  indicator-type
            flymake-fringe-indicator-position        fringe-indicator-position
            flymake-mode-line-format                 mode-line-format
            flymake-show-diagnostics-at-end-of-line  show-eol)
      (when suppress-underlines
        (dolist (type '(flymake-error flymake-warning flymake-note))
          (push '(face . nil) (get type 'flymake-overlay-control))))
      (when suppress-flymake-proc
        (with-eval-after-load 'flymake-proc
          (remove-hook 'flymake-diagnostic-functions
                       'flymake-proc-legacy-flymake))))))



(provide 'lsp-utils-setup)
;;; lsp-utils-setup.el ends here
