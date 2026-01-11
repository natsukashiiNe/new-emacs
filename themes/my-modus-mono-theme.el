;;; my-modus-mono-them.el --- Monochrome theme -*- lexical-binding: t; -*-

;;; Commentary:
;; (Almost) monochrome theme based on the modus-operandi-tinted

;;; Code:
(deftheme my-modus-mono
  "A minimal and monochrome-inspired override for modus-operandi-tinted.")

;; Define color constants (mimicking your nvim color approach):
(defconst exu-fg          "#000000")  ;; base foreground
(defconst exu-bg          "#FDF6E3")  ;; base background
(defconst exu-red         "#990000")  ;; strong accent for keywords
(defconst exu-lblue       "#023E8A")
(defconst exu-blue        "#0031a9")
(defconst exu-grey        "#5e5e5e")
(defconst exu-comment     "#5e5e5e")  ;; same as exu-grey for minimal style
(defconst exu-italic      'italic)
(defconst exu-bold        'bold)


(defconst exu-bg-pink          "#F1D5D0")  ;; soft bg color

;; DIFFERENT BGS
(defconst exu-bg3 "#efe9dd")

;; DIFFRENT FGS
(defconst exu-fg2 "#333333")

;; DIAGNOSTICS COLOR
(defconst exu-green-accent     "#60DC60")
(defconst exu-green-dimmer     "#0B5E30")
(defconst exu-bg-green         "#C7EAD9")

(defconst exu-orange-accent    "#FF8700")
(defconst exu-orange-light     "#A64700")
(defconst exu-bg-orange        "#FFD0A0")

(defconst exu-blue-light       "#1565C0") ;; "#1E88E5","#0D47A1"
(defconst exu-bg-blue          "#D7ECFF")



;; First, load the base theme:
(load-theme 'modus-operandi-tinted t)

;; Now override syntax and LSP faces:
(custom-theme-set-faces 'my-modus-mono
  ;; DEFAULT AND UI
 `(default ((t  (:background ,exu-bg))))
 `(hl-line ((t  (:background unspecified :extend t))))
 `(tab-bar ((t  (:foreground ,exu-red :background ,exu-bg3 :box nil ))))
 `(tab-bar-tab ((t  (:foreground ,exu-red :background ,exu-bg3 :box nil ))))
 `(tab-bar-inactive ((t  (:foreground ,exu-fg2 :background ,exu-bg3 :box nil ))))



 `(font-lock-function-name-face ((t  (:foreground ,exu-fg :slant ,exu-italic))))
 `(font-lock-builtin-face       ((t  (:foreground ,exu-blue :slant ,exu-italic))))
 `(font-lock-type-face          ((t  (:foreground ,exu-fg :weight ,exu-bold))))
 `(font-lock-variable-name-face ((t  (:foreground ,exu-fg))))
 `(font-lock-keyword-face       ((t  (:foreground ,exu-red :weight ,exu-bold))))
 `(font-lock-string-face        ((t  (:foreground ,exu-red :slant ,exu-italic))))
 `(font-lock-comment-face       ((t  (:foreground ,exu-comment :slant ,exu-italic))))
 `(font-lock-constant-face      ((t  (:foreground ,exu-fg :weight ,exu-bold ))))
 `(font-lock-number-face        ((t  (:foreground ,exu-lblue :slant ,exu-italic ))))


 ;; LSP faces
 ;; Errors, warnings, etc. can be minimal or softly highlighted
 `(lsp-face-error       ((t  (:foreground ,exu-red :background unspecified :weight ,exu-bold))))
 `(lsp-face-warning     ((t  (:foreground ,exu-red :background unspecified :slant ,exu-italic))))
 `(lsp-face-info        ((t  (:foreground ,exu-grey :slant ,exu-italic))))
 `(lsp-face-hint        ((t  (:foreground ,exu-grey :slant ,exu-italic))))


 `(lsp-face-hint        ((t  (:foreground ,exu-grey :slant ,exu-italic))))

 `(lsp-face-highlight-read ((t (:background unspecified ))))
 `(lsp-face-highlight-textual ((t (:background unspecified ))))
 `(lsp-face-highlight-write ((t (:background unspecified ))))   ;; LSP semantic tokens

 `(lsp-face-semhl-function   ((t  (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-member   ((t  (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-variable   ((t  (:inherit font-lock-variable-name-face))))
 `(lsp-face-semhl-type       ((t  (:inherit font-lock-type-face))))
 `(lsp-face-semhl-parameter  ((t  (:foreground ,exu-fg :slant ,exu-italic))))
 `(lsp-face-semhl-operator   ((t  (:inherit font-lock-keyword-face))))
 `(lsp-face-semhl-string     ((t  (:inherit font-lock-string-face))))
 `(lsp-face-semhl-comment    ((t  (:inherit font-lock-comment-face))))
 `(lsp-face-semhl-namespace  ((t  (:inherit font-lock-type-face))))
 `(lsp-face-semhl-constant   ((t  (:inherit font-lock-constant-face))))
 ;; etc. as needed
 `(tree-sitter-hl-face:method.call ((t (:inherit font-lock-function-name-face))))

 ;; --- FLYCHECK ----------------------------------------------------------------
 ;; inline
 `(flycheck-error   ((t (:background ,exu-bg-green  :underline nil :weight ,exu-bold))))
 `(flycheck-warning ((t (:background ,exu-bg-orange :underline nil ))))
 `(flycheck-info    ((t (:background ,exu-bg-blue   :underline nil ))))

 `(flycheck-inline-error   ((t (:foreground ,exu-green-dimmer :background ,exu-bg-green :weight ,exu-bold))))
 `(flycheck-inline-warning ((t (:foreground ,exu-orange-light :background ,exu-bg-orange ))))
 `(flycheck-inline-info    ((t (:foreground ,exu-blue-light   :background ,exu-bg-blue ))))

 ;; icons
 `(flycheck-fringe-error   ((t (:foreground ,exu-green-accent  :background ,exu-bg-orange))))
 `(flycheck-fringe-warning ((t (:foreground ,exu-orange-accent :background ,exu-bg-orange))))
 `(flycheck-fringe-info    ((t (:foreground ,exu-blue-light    :background ,exu-bg-orange))))

 ;; --- TELEGA ------------------------------------------------------------------
`(telega-msg-heading ((t (:background ,exu-bg :weight bold))))
`(telega-msg-inline-forward ((t (:background ,exu-bg :slant italic))))

 )
;; --- CUSTOM FACES ------------------------------------------------------------
(defface my-hl-line-normal
  '((t (:background "#F1D5D0" :extend t)))
  "HL line face for Evil normal mode.")

(defface my-hl-line-insert
  '((t (:background "#C7EAD9" :extend t)))
  "HL line face for Evil insert mode.")

(defun evil-hl-line--use (face)
  "Switch `hl-line-face' to FACE and refresh the overlay immediately."
  (setq hl-line-face face)
  ;; Force the existing overlay (if any) to pick up the new face even
  ;; before the point moves.
  (hl-line-highlight))



;; --- HOOKS -------------------------------------------------------------------
;; (defun my-modus-mono--enter-insert-state ()
;;   "Change cursor/line face for Evil insert state."
;;   (when (eq (car custom-enabled-themes) 'my-modus-mono)
;;     (custom-theme-set-faces
;;      'my-modus-mono
;;      '(hl-line ((t (:background ,exu-bg-green :extend t)))))
;;     (setq hl-line-face 'my-hl-line-insert)
;;     (set-face-background 'hl-line exu-bg-green)))

;; (defun my-modus-mono--enter-normal-state ()
;;   "Change cursor/line face for Evil normal state."
;;   (when (eq (car custom-enabled-themes) 'my-modus-mono)
;;     (custom-theme-set-faces
;;      'my-modus-mono
;;      '(hl-line ((t (:background ,exu-bg-pink :extend t)))))
;;     (setq hl-line-face 'my-hl-line-normal)
;;     (set-face-background 'hl-line exu-bg-pink)))


;; ;; --- APPLY HOOKS --------------------------------------------------------------
;; (defun my-modus-mono--apply-evil-hooks ()
;;   "Attach Evil hooks to tweak cursor/line highlighting when my-modus-mono is active."
;;   (add-hook 'evil-normal-state-entry-hook #'my-modus-mono--enter-normal-state)
;;   (add-hook 'evil-insert-state-entry-hook #'my-modus-mono--enter-insert-state))

;; (defun my-modus-mono--remove-evil-hooks ()
;;   "Detach Evil hooks, restoring original states."
;;   (remove-hook 'evil-normal-state-entry-hook #'my-modus-mono--enter-normal-state)
;;   (remove-hook 'evil-insert-state-entry-hook #'my-modus-mono--enter-insert-state))

;; ;; --- ENABLE/DISABLE HOOKS FOR THE THEME ---------------------------------------
;; (defun my-modus-mono--post-enable-theme (theme &rest _)
;;   (when (eq theme 'my-modus-mono)
;;     (global-hl-line-mode 1)
;;     (setq hl-line-face 'my-hl-line-normal)
;;     (my-modus-mono--apply-evil-hooks)))
;; (advice-add 'enable-theme :after #'my-modus-mono--post-enable-theme)

;; (defun my-modus-mono--post-disable-theme (theme &rest _)
;;   (when (eq theme 'my-modus-mono)
;;     (my-modus-mono--remove-evil-hooks)))
;; (advice-add 'disable-theme :after #'my-modus-mono--post-disable-theme)

(provide-theme 'my-modus-mono)
;;; my-modus-mono-theme.el ends here
