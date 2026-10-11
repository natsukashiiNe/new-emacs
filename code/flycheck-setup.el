;;; flycheck-setup.el --- Flycheck and diagnostic display setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete diagnostic setup with:
;; - Flycheck core (works with LSP via lsp-diagnostics-provider)
;; - quick-peek (inline error display framework)
;; - flycheck-inline (integrates flycheck with quick-peek)
;; - flyover (end-of-line diagnostic display)
;; - Evil integration (disable in insert state)

;;; Code:

;; =============================================================================
;; FLYCHECK - Core Diagnostic Framework
;; =============================================================================
(require 'lsp-utils-setup)
(require 'fringe-symbols-settings)

(use-package flycheck
  :ensure t
  :hook
  (lsp-mode . flycheck-mode)
  :init
  (defvar my/flycheck-idle-change-delay-insert 5
    "`flycheck-idle-change-delay' to use while in evil insert state.")

  (defvar-local my/flycheck--idle-change-delay-orig nil
    "Buffer-local `flycheck-idle-change-delay' saved before entering insert state.
Non-nil only while the raised value from
`my/flycheck-raise-idle-delay-on-insert' is in effect.")

  (defun my/flycheck-raise-idle-delay-on-insert ()
    "Raise `flycheck-idle-change-delay' to `my/flycheck-idle-change-delay-insert'
for the duration of insert state.  Stashes the current buffer-local value so
it can be restored by `my/flycheck-restore-idle-delay-on-insert-exit'."
    (when (bound-and-true-p flycheck-mode)
      (setq-local my/flycheck--idle-change-delay-orig flycheck-idle-change-delay)
      (setq-local flycheck-idle-change-delay my/flycheck-idle-change-delay-insert)))

  (defun my/flycheck-restore-idle-delay-on-insert-exit ()
    "Restore `flycheck-idle-change-delay' saved on insert-state entry."
    (when (and (bound-and-true-p flycheck-mode)
               my/flycheck--idle-change-delay-orig)
      (setq-local flycheck-idle-change-delay my/flycheck--idle-change-delay-orig)
      (setq-local my/flycheck--idle-change-delay-orig nil)))
  :config

  ;; == Check buffers ===========================================================
  (setq flycheck-check-syntax-automatically '(idle-change idle-buffer-switch
							  save mode-enabled new-line)
	flycheck-idle-change-delay 1.75
	flycheck-idle-buffer-switch-delay 2
	flycheck-display-errors-delay 1)

  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-entry-hook #'my/flycheck-raise-idle-delay-on-insert)
    (add-hook 'evil-insert-state-exit-hook #'my/flycheck-restore-idle-delay-on-insert-exit))
  
  ;; == See errors in buffers ===================================================
  (setq flycheck-checker-error-threshold 2000)
  (setq flycheck-highlighting-mode 'symbols ;; how to guess errors region
	flycheck-highlighting-style         ;; how to hi when checker repors region
	'(conditional 2	level-face (delimiters "" "")))

  (add-hook 'flycheck-mode-hook #'flycheck-annotate-mode)
  (setq flycheck-annotate-current-line-style 'below
	flycheck-annotate-other-lines-style  'sideline
	;; `t' (not an explicit level list) so the filter also passes LSP
	;; diagnostics that carry a tag (e.g. pyright's "unused" hints):
	;; those get dynamically-generated levels like
	;; `lsp-flycheck-info-unnecessary' (see
	;; `lsp-diagnostics--flycheck-level'), which never `memq' into an
	;; explicit '(error warning info) list.
	flycheck-annotate-levels t
	flycheck-annotate-current-line-levels t
	flycheck-annotate-other-lines-levels '(error)
	flycheck-annotate-suppress-echo t
	flycheck-annotate-background nil
	flycheck-annotate-fix-marker "[fix]" ;; TODO: flycheck-annotate-fix face
	)

  (setq flycheck-indication-mode 'auto)
  ;; TODO: fixable fringe && margin:
  ;; flycheck-fixable-indicator &&  flycheck-fixable-margin-str
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap (my-fringe/bitmap 'diagnostics-error)
    :fringe-face (my-fringe/face 'diagnostics-error)
    :error-list-face 'flycheck-error-list-error)
  (flycheck-define-error-level 'warning
    :severity 10
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap (my-fringe/bitmap 'diagnostics-warning)
    :fringe-face (my-fringe/face 'diagnostics-warning)
    :error-list-face 'flycheck-error-list-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap (my-fringe/bitmap 'diagnostics-info)
    :fringe-face (my-fringe/face 'diagnostics-info)
    :error-list-face 'flycheck-error-list-info))


;; LSP integration - use flycheck as diagnostics provider
(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flycheck))

;; =============================================================================
;; QUICK-PEEK - Inline Display Framework
;; =============================================================================

(use-package quick-peek
  :ensure t
  :config
  ;; Configure display settings
  (setq quick-peek-add-spacer nil)
  
  ;; Fix face to properly extend background color
  (set-face-attribute 'quick-peek-background-face nil
		      :background nil
		      :inherit 'default
		      :extend t))

;; =============================================================================
;; ADDITIONAL FLYCHECK CHECKERS
;; =============================================================================

;; Emacs Lisp specific checkers
(use-package flycheck-package
  :ensure t
  :after flycheck
  :config
  (flycheck-package-setup))


(use-package consult-flycheck
  :ensure t
  :after flycheck consult)

;; =============================================================================
;; DIAGNOSTIC KEYBINDINGS
;; =============================================================================

(with-eval-after-load 'flycheck
  ;; Global keybindings for navigating 
  (global-set-key (kbd "C-j") 'flycheck-next-error)
  (global-set-key (kbd "C-k") 'flycheck-previous-error)
  
  ;; Flycheck-specific commands
  (global-set-key (kbd "C-c ! l") 'flycheck-list-errors)
  (global-set-key (kbd "C-c ! n") 'flycheck-next-error)
  (global-set-key (kbd "C-c ! p") 'flycheck-previous-error)
  (global-set-key (kbd "C-c ! c") 'flycheck-buffer)
  (global-set-key (kbd "C-c ! C") 'flycheck-clear))

(provide 'flycheck-setup)
;;; flycheck-setup.el ends here
