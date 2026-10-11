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
  :defer t
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
  :defer t
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ("C-c l s" . consult-lsp-symbols)        ; Workspace symbols
              ("C-c l d" . consult-lsp-diagnostics)    ; All diagnostics
              ("C-c l f" . consult-lsp-file-symbols))) ; File symbols

;; === Vertico =========================================================

(use-package vertico
  :ensure t
  :demand t
  :after savehist
  :config
  (vertico-mode 1)
  ;; Persist across Emacs sessions
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  (unless (display-graphic-p)
    (set-face-attribute 'vertico-current nil :inverse-video t))

  (defvar my/vertico-count--session-default nil
    "`vertico-count' value to restore after this minibuffer session.
Non-nil only while the count has been resized mid-session; see
`my/vertico-count-adjust' and `my/vertico-count--restore-once'.")

  (defun my/vertico-count--restore-once ()
    "Restore `vertico-count' once, then remove self from the hook.
Resizing is only ever meant to last for the session it happened in;
the next minibuffer command should start from the usual count again."
    (remove-hook 'minibuffer-exit-hook #'my/vertico-count--restore-once)
    (when my/vertico-count--session-default
      (setq vertico-count my/vertico-count--session-default
            my/vertico-count--session-default nil)))

  (defun my/vertico-count-adjust (n)
    "Change `vertico-count' by N for this minibuffer session only."
    (unless my/vertico-count--session-default
      (setq my/vertico-count--session-default vertico-count)
      (add-hook 'minibuffer-exit-hook #'my/vertico-count--restore-once))
    (setq vertico-count (max 1 (+ vertico-count n)))
    (vertico--exhibit))
  (defun my/vertico-count-increment (&optional n)
    "Grow `vertico-count' (candidate list height) by N (default 1)."
    (interactive "p")
    (my/vertico-count-adjust (or n 1)))
  (defun my/vertico-count-decrement (&optional n)
    "Shrink `vertico-count' (candidate list height) by N (default 1)."
    (interactive "p")
    (my/vertico-count-adjust (- (or n 1))))
  :bind
  (:map minibuffer-mode-map
	("M-RET" . vertico-exit-input )
	("C-x o" . vertico-exit-input )
	("C-d"   . vertico-scroll-up)
	("C-u"   . vertico-scroll-down)
	("M-j"   . vertico-next-group)
	("M-k"   . vertico-previous-group)
	("M-h"   . vertico-first)
	("M-l"   . vertico-last)
	;; Resolve clash with the `C-M-j/k' window-resize bindings in
	;; general-keymaps.el: those are given a `(not (minibufferp))'
	;; predicate so lookup falls through to these while the
	;; minibuffer is active.
	("C-M-j" . my/vertico-count-increment)
	("C-M-k" . my/vertico-count-decrement)))

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
	      ("C-c O" . embark-act)
	      ("C-c E" . embark-dwim)
	      ("C-c C-e" . embark-collect)
	      ("C-c C-c" . embark-export)
	      ("C-c B" . embark-become)
	      )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps hints using which-key."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "..." "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  :config
  (setq embark-indicators
	'(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  )

(use-package embark-consult
  :ensure t
  :demand t
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook
            (lambda () (when (fboundp 'consult-preview-at-point-mode)
                         (consult-preview-at-point-mode)))))


;; Pop-up consult window

;; == some completing-read help packages

;; (use-package affe
;;   :ensure t
;;   :config
;;   (defun affe-orderless-regexp-compiler (input _type _ignorecase)
;;     (setq input (cdr (orderless-compile input)))
;;     (cons input (apply-partially #'orderless--highlight input t)))
;;   (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
;;   :bind
;;   (:map global-map
;; 	("C-x C-f" . #'affe-find)
;; 	:map project-prefix-map
;; 	("f" . #'affe-find)))

(provide 'completion-setup)
;;; completion-setup.el ends here
