;;; completion-setup.el --- Startup file -*- lexical-binding: t; -*-

;;; Commentary:


;; Setup for completion: consult ecosystem.

;;; Code:


;; === CONSULT =========================================================
(use-package consult
  :ensure t
  :demand t
  :config
  (setq consult-preview-key 'any)
  (setq consult-async-min-input 0) ; Show results immediately
  (setq consult-async-refresh-delay 0.2)

  :bind (;; Minibuffer navigation
         :map minibuffer-local-map
         ("C-j" . next-line-or-history-element)
         ("C-k" . previous-line-or-history-element)))

(use-package consult-ls-git
  :ensure t
  :after consult
  :bind (("C-c g f" . consult-ls-git)))

(use-package consult-xref
  :ensure nil
  :after consult
  :demand t
  :config
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ("C-c l s" . consult-lsp-symbols)        ; Workspace symbols
              ("C-c l d" . consult-lsp-diagnostics)    ; All diagnostics
              ("C-c l f" . consult-lsp-file-symbols))) ; File symbols


;; TODO move to keymap file
(with-eval-after-load 'evil
  (keymap-set evil-normal-state-map "C-s" 'consult-line)
  (keymap-set evil-visual-state-map "C-s" 'consult-line))

(keymap-set global-map "C-c C-s y" #'consult-yank-replace)

;; === Vertico =========================================================

(use-package vertico
  :ensure t
  :demand t
  :after savehist
  :config
  (vertico-mode 1)
  ;; Persist across Emacs sessions
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  :bind
  (:map minibuffer-mode-map
	("M-RET" . vertico-exit-input )
	("C-x o" . vertico-exit-input )
	("C-d"   . vertico-scroll-up)
	("C-u"   . vertico-scroll-down)
	("M-j"   . vertico-next-group)
	("M-k"   . vertico-previous-group)
	("M-h"   . vertico-first)
	("M-l"   . vertico-last)))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :demand t
  :init
  (defcustom my/vertico-repeat-whitelist
    '(
      consult-ripgrep consult-grep consult-git-grep
      consult-imenu consult-outline consult-line
      consult-lsp-symbols consult-lsp-diagnostics
      consult-lsp-file-symbols lsp-find-references
      lsp-find-definition lsp-find-implementation xref-find-references)
    "Commands for which vertico-repeat should save sessions."
    :type '(repeat symbol)
    :group 'vertico)

  (defun my/vertico-repeat--whitelist-filter (session)
    "Only save SESSION if command is in whitelist."
    (and (memq (car session) my/vertico-repeat-whitelist) session))
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-repeat-transformers
   (list #'my/vertico-repeat--whitelist-filter))
  :bind (("C-c v" . vertico-repeat)
	 ("C-c V" . vertico-repeat-select)
	 :map vertico-map
	 ("M-p" .  vertico-repeat-previous)
	 ("M-n" . vertico-repeat-next))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal global-map
      (kbd "M-w") 'vertico-repeat)))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode 1))

;; === EMBARK =========================================================
(use-package embark
  :ensure t
  :demand t
  :bind (:map minibuffer-mode-map
	      ("C-;" . embark-act)
	      ("C-c e" . embark-act)
	      ("C-c E" . embark-dwim)
	      ("C-c C-c" . embark-collect)
	      ("C-c C-e" . embark-export)
	      ("C-c C" . embark-become)
	      )
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :demand t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; Pop-up consult window



(provide 'completion-setup)
;;; completion-setup.el ends here
