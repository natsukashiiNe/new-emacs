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

(use-package flycheck
  :ensure t
  :init
  ;; (global-flycheck-mode 1)
  :hook
  (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-fringe)
  :config
  ;; Timing settings
  (setq flycheck-idle-change-delay 0.1
        flycheck-display-errors-delay 0.1
        flycheck-check-syntax-automatically '(save mode-enabled idle-change)
        flycheck-checker-error-threshold nil)
  
  ;; Display settings (will be overridden by flycheck-inline)
  (setq flycheck-display-errors-function nil)

  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector
     #b0000011111100000)
    nil 16 '(top t))

  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)

  (flycheck-define-error-level 'warning
    :severity 10
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)

  (flycheck-define-error-level 'info
    :severity 0
    :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info
    :error-list-face 'flycheck-error-list-info))

;; ==== Using flymake to complement flycheck ====================================
(defvar my-diagnostics/flymake-presents
  '((default . (:indicator-type fringes
				:fringe-indicator-position left-fringe
				:mode-line-format (" " flymake-mode-line-title
						   flymake-mode-line-exception
						   flymake--mode-line-counters)
				:show-eol t
				:suppress-underlines nil
				:suppress-flymake-proc nil))

    (complement-flycheck . (:indicator-type nil
					    :fringe-indicator-position nil
					    :mode-line-format nil
					    :show-eol t
					    :suppress-underlines t
					    :suppress-flymake-proc t))))


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
;; FLYCHECK-INLINE - Inline Error Display
;; =============================================================================

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode)
  :config
  ;; Configure inline display with quick-peek
  (setq flycheck-inline-display-function
	(lambda (msg pos err)
	  (let* ((ov (quick-peek-overlay-ensure-at pos))
		 (contents (quick-peek-overlay-contents ov)))
	    (setf (quick-peek-overlay-contents ov)
		  (concat contents (when contents "\n") msg))
	    (quick-peek-update ov)))
	
	flycheck-inline-clear-function #'quick-peek-hide))

;; =============================================================================
;; FLYOVER - End-of-Line Diagnostic Display
;; =============================================================================

(use-package flyover
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flyover-mode)
  :custom
  (flyover-checkers '(flycheck))
  (flyover-levels '(error warning info))

  ;; Appearance
  (flyover-info-icon " ")
  (flyover-warning-icon " ")
  (flyover-error-icon " ")

  (flyover-border-style 'slant)
  (flyover-border-match-icon t)

  ;; Colors
  (flyover-use-theme-colors t)
  ;; Tinting: 'lighter, 'darker, or nil
  (flyover-text-tint 'lighter)
  (flyover-icon-tint 'lighter)
  (flyover-icon-background-tint 'lighter)
  (flyover-text-tint-percent 0)
  (flyover-icon-tint-percent 0)
  (flyover-icon-background-tint-percent 50)

  ;; Display settings
  (flyover-virtual-line-type nil)
  (flyover-show-at-eol t)
  (flyover-hide-dung-completion t)
  (flyover-display-mode 'hide-at-exact-position)

  ;; Sources settings
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line nil)

  (flyover-line-position-offset 1)

  ;; Message wrapping
  (flyover-wrap-messages t)
  (flyover-max-line-length 80)

  ;; Performance
  (flyover-debounce-interval 0.2)
  (flyover-cursor-debounce-interval 0.3)

  ;; Completion integration
  
  :config
  ;; Flyover's internal logic sometimes overrides face settings,
  ;; (defun my/flyover-use-custom-faces (level)
  ;;   "Get colors from flyover-* faces instead of base LEVEL (e/w/i) faces."
  ;;   (let* ((face (pcase level
  ;;                  ((or 'error "error") 'flyover-error)
  ;;                  ((or 'warning "warning") 'flyover-warning)
  ;;                  ((or 'info "info") 'flyover-info)
  ;;                  (_ 'flyover-warning)))
  ;;          (fg (face-attribute face :foreground nil t))
  ;;          (bg (face-attribute face :background nil t))
  ;;          (final-bg (if (eq bg 'unspecified)
  ;; 			 (flyover--create-background-from-foreground fg
  ;; 								     flyover-background-lightness)
  ;;                      bg)))
  ;;     (cons fg final-bg)))
  ;; (advice-add 'flyover--get-face-colors :override #'my/flyover-use-custom-faces)
  )

;; =============================================================================
;; EVIL INTEGRATION - Disable Inline Diagnostics in Insert State
;; =============================================================================

;; Track diagnostic mode state before entering insert state
(defvar-local my/flycheck-inline-was-active nil
  "Track if flycheck-inline was active before entering insert state.")

(defvar-local my/flyover-was-active nil
  "Track if flyover was active before entering insert state.")

(defun my/disable-inline-errors-in-insert ()
  "Disable flycheck-inline and flyover in insert state.
Stores their previous state for restoration on exit."
  (when (bound-and-true-p flycheck-mode)
    ;; Store current state
    (setq my/flycheck-inline-was-active (bound-and-true-p flycheck-inline-mode))
    (setq my/flyover-was-active (bound-and-true-p flyover-mode))
    
    ;; Disable if active
    (when my/flycheck-inline-was-active
      (flycheck-inline-mode -1))
    (when my/flyover-was-active
      (flyover-mode -1))))

(defun my/enable-inline-errors-on-exit ()
  "Re-enable flycheck-inline and flyover when exiting insert state.
Only re-enables if they were previously active."
  (when (bound-and-true-p flycheck-mode)
    ;; Restore previous state
    (when (and my/flycheck-inline-was-active
	       (not (bound-and-true-p flycheck-inline-mode)))
      (flycheck-inline-mode 1))
    (when (and my/flyover-was-active
	       (not (bound-and-true-p flyover-mode)))
      (flyover-mode 1))))

;; Hook into Evil state changes
(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'my/disable-inline-errors-in-insert)
  (add-hook 'evil-insert-state-exit-hook #'my/enable-inline-errors-on-exit))

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
