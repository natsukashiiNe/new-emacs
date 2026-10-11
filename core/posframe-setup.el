;;; posframe-setup.el --- Posframe settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup of the posframe plugins: settings, supportive plugina, its respective hotkeys.

;;; Code:

(use-package posframe
  :ensure t
  :demand t)

(use-package vertico-posframe
  :ensure t
  :demand t
  :init
  (defvar my-vertico-posframe/floating-count 25
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
      consult-flymake
      consult-flyspell
      consult-compile-error
      consult-org-heading
      consult-lsp-symbols
      consult-lsp-file-symbols
      consult-yank-replace

      evil-collection-consult-mark
      evil-collection-consult-jump-list

      minuet-complete-with-minibuffer

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

  (define-minor-mode my/vertico-posframe-auto-toggle-mode
    "Automatically disable `vertico-posframe-mode' for certain commands.

When enabled, `vertico-posframe-mode' is turned on with
`my-vertico-posframe/floating-count' candidates, and commands in
`my-vertico-posframe/disable-commands' temporarily fall back to the
plain minibuffer until it exits. Disabling this mode is a clean
switch back to the plain minibuffer: it removes the hooks, turns
`vertico-posframe-mode' off, and resets `vertico-count' to
`my-vertico-posframe/normal-count' — the default you'd get without
this package at all."
    :global t
    (if my/vertico-posframe-auto-toggle-mode
        (progn
          (add-hook 'minibuffer-setup-hook #'my/disable-vertico-posframe)
          (add-hook 'minibuffer-exit-hook #'my/enable-vertico-posframe)
          (vertico-posframe-mode 1)
          (setq vertico-count my-vertico-posframe/floating-count))
      (remove-hook 'minibuffer-setup-hook #'my/disable-vertico-posframe)
      (remove-hook 'minibuffer-exit-hook #'my/enable-vertico-posframe)
      (vertico-posframe-mode -1)
      (setq vertico-count my-vertico-posframe/normal-count)))

  (my/vertico-posframe-auto-toggle-mode 1))

;; TODO: make it toggle
(keymap-set minibuffer-mode-map "C-c C" (li (vertico-posframe-mode 'toggle)))

(defun my/posframe-release-surrogate-minibuffer (&optional frame &rest _)
  "Delete any posframe child frame still borrowing FRAME's minibuffer window.

Posframe caches its popup frame and only refreshes its `parent-frame'
parameter on reuse, never `minibuffer' (set once at creation to the
then-current parent's minibuffer window). Under the daemon, the single
cached popup frame is shared by every client frame, so it can end up
parented to one frame while still registered as surrogate minibuffer
frame of another, which makes `delete-frame' on that other frame fail
with \"Attempt to delete a surrogate minibuffer frame\"."
  (let ((frame (or frame (selected-frame))))
    (dolist (f (frame-list))
      (when (and (frame-parameter f 'posframe-buffer)
                 (eq (window-frame (minibuffer-window f)) frame))
        (posframe-delete-frame (car (frame-parameter f 'posframe-buffer)))))))

(advice-add 'delete-frame :before #'my/posframe-release-surrogate-minibuffer)

(provide 'posframe-settings)
;;; posframe-settings ends here
