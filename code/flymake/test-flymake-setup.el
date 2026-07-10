;;; test-flymake-setup.el --- Standalone flymake test config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; == FLYMAKE BACKENDS ===========================================================

;; -- python: use ruff via lsp directly -----------------------------------------
;; -- TODO: move to python dedicated file
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff" "server"))
    :activation-fn (lsp-activate-on "python")
    :server-id 'ruff-lsp
    :priority -2
    :add-on? t)))

;; -- bash: built-in to emacs 29 ------------------------------------------------
(when (require 'flymake-shellcheck nil t)
  (add-hook 'sh-mode-hook #'flymake-shellcheck-load)
  (add-hook 'bash-ts-mode-hook #'flymake-shellcheck-load))

;; -- elisp --------------------------------------------------------------------
(use-package package-lint-flymake
  :ensure t
  :hook (emacs-lisp-mode . package-lint-flymake-setup))

;; fix load-path for byte-compile checks in config files
(use-package flymake-elisp-config
  :ensure t
  :hook (emacs-lisp-mode . flymake-elisp-config-mode))

;; == CONSULT-FLYMAKE (if available) ============================================

(with-eval-after-load 'consult
  (when (locate-library "consult-flymake")
    (autoload 'consult-flymake "consult-flymake" nil t)))


;; == FLYMAKE SETTINGS ==========================================================

(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flymake))

(require 'flymake)
(setq flymake-wrap-around t)

;; -- Fringe indicators ---------------------------------------------------------
(define-fringe-bitmap 'my-flymake/fringe-indicator
  (vector #b0000011111100000)
  nil 16 '(top t))

(setq flymake-indicator-type 'fringes)
(setq flymake-fringe-indicator-position 'left-fringe)
(setq flymake-error-bitmap   '(my-flymake/fringe-indicator flymake-error-fringe))
(setq flymake-warning-bitmap '(my-flymake/fringe-indicator flymake-warning-fringe))
(setq flymake-note-bitmap    '(my-flymake/fringe-indicator flymake-note-fringe))

;; -- Suppress flymake-proc legacy backend -------------------------------------
(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; -- Activate in prog-mode ----------------------------------------------------
(setq flymake-start-on-flymake-mode t)
(setq flymake-start-on-save-buffer t)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'text-mode-hook #'flymake-mode)  ; for prose linting

(require 'flymake-diagnostics)

(provide 'test-flymake-setup)
;;; test-flymake-setup.el ends here
