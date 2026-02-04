;;; posframe-setup.el --- Posframe settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup of the posframe plugins: settings, supportive plugina, its respective hotkeys.

;;; Code:

(use-package posframe
  :ensure t)

(use-package vertico-posframe
  :ensure t
  :init
  (defvar my-vertico-posframe/floating-count 20
    "Number of vertico candidate to show in posframe mode")
  (defvar my-vertico-posframe/normal-count 12
    "Number of vertico candidate to show in posframe mode")
  (defvar my-vertico-posframe/disable-commands
    '(projectile-grep
      project-find-regexp
      lsp-find-references
      lsp-find-definition
      xref-find-references
      consult-fd
      consult-grep
      consult-buffer
      consult-ripgrep
      consult-project-buffer
      consult-ls-git
      my-consult-projectile-find-file

      consult-xref
      consult-lsp-diagnostics
      consult-lsp-file-symbols

      consult-info

      consult-line
      consult-imenu
      consult-outline
      consult-flycheck
      consult-flyspell
      consult-compile-error
      consult-org-heading
      consult-lsp-symbols
      consult-lsp-file-symbols
      consult-yank-replace

      evil-collection-consult-mark
      evil-collection-consult-jump-list

      my/consult-definitions
      my/elastic-files-consult)
    "Commands for which vertico-posframe should be disabled")
  :after (vertico posframe)
  :custom

  ;; Position handler
  (vertico-posframe-poshandler 'posframe-poshandler-frame-center)
  (vertico-posframe-border-width 10)
  ;; Optional extra posframe parameters
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)
     ))
  :config
  (setq vertico-count my-vertico-posframe/floating-count)

  (defun my/disable-vertico-posframe ()
    "Disable vertico-posframe for certain commands."
    (when (memq this-command my-vertico-posframe/disable-commands)
      (vertico-posframe-mode -1)
      (setq vertico-count my-vertico-posframe/normal-count)))

  (defun my/enable-vertico-posframe ()
    "Re-enable vertico-posframe after minibuffer exits."
    (unless vertico-posframe-mode
      (vertico-posframe-mode 1)
      (setq vertico-count my-vertico-posframe/floating-count)))

  (add-hook 'minibuffer-setup-hook #'my/disable-vertico-posframe)
  (add-hook 'minibuffer-exit-hook #'my/enable-vertico-posframe)

  (vertico-posframe-mode 1))


(provide 'posframe-settings)
;;; posframe-settings ends here
