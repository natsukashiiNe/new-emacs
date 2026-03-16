;;; my-modus-mono-theme.el --- Monochrome theme -*- lexical-binding: t; -*-

;;; Commentary:
;; (Almost) monochrome theme based on the modus-operandi-tinted

;;; Code:
(deftheme my-modus-mono
  "A minimal and monochrome-inspired override for modus-operandi-tinted.")

;; Define color constants (mimicking your nvim color approach):
(defconst exu-black       "#000000")  ;; black
(defconst exu-fg          "#000000")  ;; base foreground
(defconst exu-bg          "#EBDDB1")  ;; base background
(defconst exu-red         "#990000")  ;; strong accent for keywords
(defconst exu-lblue       "#304483")
(defconst exu-blue        "#0031a9")
(defconst exu-blue-accent "#0000FF")
(defconst exu-grey        "#5e5e5e")
(defconst exu-comment     "#5e5e5e")  ;; same as exu-grey for minimal style
(defconst exu-italic      'italic)
(defconst exu-bold        'bold)


(defconst exu-bg-pink          "#F5B0B0")  ;; soft bg color

;; DIFFERENT BGS
(defconst exu-bg-dim1 "#E9E5BB")
(defconst exu-bg-dim2 "#DBCC97")
(defconst exu-bg3 "#CAB9B2")

;; DIFFRENT FGS
(defconst exu-fg2 "#333333")

;; DIAGNOSTICS COLOR
;; -- Error
(defconst exu-green-accent     "#60DC60")
(defconst exu-green-dimmer     "#0B5E30")
(defconst exu-bg-green         "#C7EAD9")

;; -- Warn
(defconst exu-orange-accent    "#FF8700")
(defconst exu-orange-light     "#A64700")
(defconst exu-orange-dimmer    "#A0422E")
(defconst exu-bg-orange        "#FFD0A0")

;; -- Info
(defconst exu-blue-light       "#1565C0") ;; "#1E88E5","#0D47A1"
(defconst exu-bg-blue          "#D7ECFF")

(defconst exu-purple-dimmer       "#8E24AA") ;; "#1E88E5","#0D47A1"
(defconst exu-purple-bg          "#F0EEFF")



;; First, load the base theme:
(load-theme 'modus-operandi-tinted t)

;; Now override syntax and LSP faces:
(custom-theme-set-faces
 'my-modus-mono

 ;; --- CORE --------------------------------------------------------------------
 `(cursor  ((t  (:background ,exu-red))))
 `(hl-line ((t  (:background ,exu-bg-dim2 :extend t))))
 ;; TODO: light variant for current line number
 `(line-number-current-line ((t (:foreground "#FFFFFF" :background "#870000" :slant normal :weight normal))))
 `(show-paren-match ((t  ( :foreground ,exu-green-dimmer :background ,exu-bg-green :weight bold :extend t))))
 `(my/hl-paren-middle    ((t (:foreground ,exu-blue-light :background ,exu-bg-blue))))
 `(my/hl-paren-innermost ((t (:background ,exu-purple-dimmer :foreground ,exu-bg-pink :weight bold))))

 `(hl-sexp-face          (( t (:background ,exu-bg-dim2))))
 `(hi    (( t (:foreground ,exu-blue-light :background ,exu-bg-blue :weight bold :slant normal))))

 `(hl-line-insert ((t  (:background ,exu-bg-pink :extend t))))

 `(default      ((t  (:background ,exu-bg))))
 ;; TODO: light variant for line-number
 `(line-number  ((t  (:background ,exu-bg-dim2))))
 ;;  TODO: light variant for fringe
 `(fringe       ((t  (:background ,exu-bg-dim2))))

 ;; --- DEFAULT AND UI ----------------------------------------------------------
 ;; TODO: light variant for avy
 `(avy-lead-face   ((t  (:foreground ,exu-bg :background ,exu-red :weight normal))))
 `(avy-lead-face-0 ((t  (:foreground ,exu-bg :background ,exu-red :weight normal))))
 `(avy-lead-face-1 ((t  (:foreground ,exu-bg :background ,exu-red :weight normal))))
 `(avy-lead-face-2 ((t  (:foreground ,exu-bg :background ,exu-red :weight normal))))

 ;; TODO: light variant for help faces
 ;; `(help-key-binding   ((t  (:foreground ,exu-bg :background ,exu-red :weight normal))))
 ;; `(help-argument-name ((t  (:foreground ,exu-red :weight bold))))

 ;; TODO: light variant for button
 ;; `(button            ((t  (:foreground ,exu-blue :slant italic :weight normal :underline (:color ,exu-blue)))))
 `(minibuffer-prompt ((t  (:foreground ,exu-bg :background ,exu-red :box nil))))
 ;; TODO: light variant for borders
 `(border            ((t  (:foreground ,exu-red :background ,exu-bg :box nil))))
 `(internal-border   ((t  (:foreground ,exu-red))))
 `(vertico-current   ((t  (:background ,exu-bg-pink :weight normal :slant normal))))

 `(orderless-match-face-0   ((t  (:foreground ,exu-blue-light :background ,exu-bg-blue :weight bold :slant normal))))
 `(orderless-match-face-1   ((t  (:foreground ,exu-green-dimmer :background ,exu-bg-green :weight bold :slant normal))))
 `(orderless-match-face-2   ((t  (:background ,exu-purple-bg :foreground ,exu-purple-dimmer :weight bold :slant normal))))
 `(orderless-match-face-3   ((t  (:foreground ,exu-orange-dimmer :background ,exu-bg-orange :weight bold :slant normal))))

 ;; TODO: light variant for marginalia
 `(marginalia-documentation ((t  (:foreground ,exu-blue :slant ,exu-italic :weight normal))))
 `(marginalia-key           ((t  (:foreground ,exu-red  :slant ,exu-italic :weight normal))))

 `(tab-bar              ((t  (:foreground ,exu-fg2 :background ,exu-bg3 :weight normal
					  :box (:line-width (-1 . 1) :color ,exu-black :style flat-button)))))
 `(tab-bar-tab          ((t  (:foreground ,exu-red :background ,exu-bg3 :weight bold
					  :box (:line-width (-1 . 1) :color ,exu-black :style flat-button)))))
 `(tab-bar-tab-inactive ((t  (:foreground ,exu-fg2 :background ,exu-bg3 :weight normal
					  :box (:line-width (-1 . 1) :color ,exu-black :style flat-button)))))

 ;; --- FONT-LOCK ---------------------------------------------------------------
 `(font-lock-function-name-face ((t  (:foreground ,exu-fg :slant ,exu-italic))))
 `(font-lock-function-call-face ((t  (:foreground ,exu-fg :slant ,exu-italic))))
 `(font-lock-builtin-face       ((t  (:foreground ,exu-blue :slant ,exu-italic))))
 `(font-lock-type-face          ((t  (:foreground ,exu-fg :weight ,exu-bold))))
 `(font-lock-variable-name-face ((t  (:foreground ,exu-fg))))
 `(font-lock-variable-use-face  ((t  (:foreground ,exu-fg))))
 `(font-lock-property-name-face ((t  (:foreground ,exu-fg))))
 `(font-lock-property-use-face  ((t  (:foreground ,exu-fg))))
 `(font-lock-keyword-face       ((t  (:foreground ,exu-red :weight ,exu-bold))))
 `(font-lock-string-face        ((t  (:foreground ,exu-red :slant ,exu-italic))))
 `(font-lock-negation-char-face ((t  (:inherit font-lock-escape-face))))
 `(font-lock-comment-face       ((t  (:foreground ,exu-comment :slant ,exu-italic))))
 ;; TODO: light variant for doc faces
 `(font-lock-doc-face           ((t  (:foreground ,exu-lblue :slant ,exu-italic))))
 `(font-lock-doc-markup-face    ((t  (:foreground "#4020CC" :slant normal :extend t))))
 `(font-lock-constant-face      ((t  (:foreground ,exu-fg :weight ,exu-bold))))
 `(font-lock-number-face        ((t  (:foreground ,exu-blue :slant ,exu-italic))))

 ;; --- LSP FACES ---------------------------------------------------------------
 ;; TODO: light variant for symbol-overlay
 `(symbol-overlay-default-face ((t :background "#f6c3cf" :foreground unspecified :weight normal
				   :box (:line-width (1 . -1) :color ,exu-black :style flat-button))))

 ;; TODO: light variant for lsp highlight (read/textual/write with visible bg)
 ;; `(lsp-face-highlight-textual ((t (:background ,exu-bg-dim1 :foreground unspecified :weight bold))))
 ;; `(lsp-face-highlight-read    ((t (:background ,exu-bg-dim1 :foreground unspecified :weight bold))))
 ;; `(lsp-face-highlight-write   ((t (:background ,exu-bg-dim1 :foreground unspecified :weight bold))))

 `(lsp-face-error       ((t  (:foreground ,exu-red :background unspecified :weight ,exu-bold))))
 `(lsp-face-warning     ((t  (:foreground ,exu-red :background unspecified :slant ,exu-italic))))
 `(lsp-face-info        ((t  (:foreground ,exu-grey :slant ,exu-italic))))
 `(lsp-face-hint        ((t  (:foreground ,exu-grey :slant ,exu-italic))))

 `(lsp-face-highlight-read ((t (:background unspecified))))
 `(lsp-face-highlight-textual ((t (:background unspecified))))
 `(lsp-face-highlight-write ((t (:background unspecified))))

 ;; LSP semantic tokens
 `(lsp-face-semhl-function   ((t  (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-member     ((t  (:inherit font-lock-function-name-face))))
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
 ;; TODO: light variant for base success/error/warning/info faces
 `(success ((t (:foreground ,exu-purple-dimmer :background ,exu-purple-bg :weight bold))))
 `(error   ((t (:foreground ,exu-green-dimmer :background ,exu-bg-green :weight bold :extend t))))
 `(warning ((t (:foreground ,exu-orange-light :background ,exu-bg-orange :weight ,exu-bold :extend t))))
 `(info    ((t (:foreground ,exu-blue-light   :background ,exu-bg-blue))))

 ;; diagnostics
 `(flycheck-error   ((t (:background ,exu-bg-green  :underline nil :weight ,exu-bold))))
 `(flycheck-warning ((t (:background ,exu-bg-orange :underline nil))))
 `(flycheck-info    ((t (:background ,exu-bg-blue   :underline nil))))

 ;; inline
 `(flycheck-inline-error   ((t (:foreground ,exu-green-dimmer :background ,exu-bg-green :weight ,exu-bold))))
 `(flycheck-inline-warning ((t (:foreground ,exu-orange-light :background ,exu-bg-orange))))
 `(flycheck-inline-info    ((t (:foreground ,exu-blue-light   :background ,exu-bg-blue))))

 ;; fringe icons
 `(flycheck-fringe-error   ((t (:foreground ,exu-green-accent  :background ,exu-bg-orange))))
 `(flycheck-fringe-warning ((t (:foreground ,exu-orange-accent :background ,exu-bg-orange))))
 `(flycheck-fringe-info    ((t (:foreground ,exu-blue-light    :background ,exu-bg-orange))))

 ;; TODO: light variant for flyover
 `(flyover-error   ((t (:foreground ,exu-green-dimmer :background ,exu-bg-green :extend t))))
 `(flyover-warning ((t (:foreground ,exu-orange-light :background ,exu-bg-orange :extend t))))
 `(flyover-info    ((t (:foreground ,exu-blue-light   :background ,exu-bg-blue :extend t))))

 ;; TODO: light variant for flycheck-overlay-marker
 ;; `(flycheck-overlay-marker ((t (:foreground ,exu-green-accent :background ,exu-bg-orange))))

 ;; --- GIT ---------------------------------------------------------------------
 ;; TODO: light variant for magit faces
 ;; `(magit-diff-file-heading ((t (:foreground ,exu-blue :weight normal))))
 ;; `(magit-filename          ((t (:foreground ,exu-red :weight normal))))
 ;; `(magit-hash              ((t (:foreground ,exu-grey :weight normal))))
 ;; `(magit-branch-local      ((t (:foreground ,exu-red :weight normal))))
 ;; `(magit-branch-remote     ((t (:foreground ,exu-red :weight normal))))

 ;; TODO: light variant for diff-hl
 `(diff-hl-insert ((t (:foreground "#37712F" :background ,exu-bg-dim2))))
 `(diff-hl-change ((t (:foreground "#485496" :background ,exu-bg-dim2))))
 `(diff-hl-delete ((t (:foreground "#914834" :background ,exu-bg-dim2))))

 ;; --- DIRED -------------------------------------------------------------------
 ;; TODO: light variant for dirvish/dired
 ;; `(dirvish-hl-line   ((t (:foreground ,exu-bg :background ,exu-red :weight normal))))
 `(dired-directory   ((t (:foreground ,exu-black :weight bold))))
 ;; `(dired-   ((t (:foreground ,exu-black :weight bold))))
 ;; `(dirvish-file-size ((t (:foreground ,exu-grey :weight normal))))

 ;; --- COMPILATION -------------------------------------------------------------
 ;; TODO: light variant for comint
 ;; `(comint-highlight-prompt ((t (:foreground ,exu-bg :background ,exu-blue :inherit nil))))

 `(compilation-info ((t (:foreground ,exu-fg))))
 `(compilation-line-number ((t (:foreground ,exu-fg))))
 `(compilation-column-number ((t (:foreground ,exu-fg))))
 
 `(fancy-compilation-default-face ((t (:foreground ,exu-fg))))
 `(fancy-compilation-column-number-face ((t (:foreground ,exu-bg :background ,exu-blue :inherit nil))))

 `(fancy-compilation-function-name-face ((t (:foreground ,exu-purple-dimmer :inherit nil))))
 `(fancy-compilation-info-face          ((t (:foreground "#0000FF" :weight bold :underline nil ))))
 `(fancy-compilation-line-number-face   ((t (:foreground ,exu-fg :weight bold ))))
 `(fancy-compilation-warning-face       ((t (:inherit warning))))
 `(fancy-compilation-error-face         ((t (:inherit error))))

 ;; --- MODELINE ----------------------------------------------------------------
 ;; TODO: light variant for doom-modeline
 `(doom-modeline-buffer-modified   ((t (:foreground ,exu-blue-light :background ,exu-bg-blue :weight bold
						    :box (:line-width (4 . -4) :color ,exu-black :style flat-button)))))
 `(mode-line-inactive      ((t (:background ,exu-bg :foreground unspecified
					    :box (:line-width (-1 . 1) :color ,exu-black :style flat-button)))))
 ;; `(doom-modeline-evil-insert-state ((t (:background ,exu-orange-light :foreground ,exu-bg-orange))))

 ;; --- TELEGA ------------------------------------------------------------------
 `(telega-msg-heading ((t (:background ,exu-bg :weight bold))))
 `(telega-msg-inline-forward ((t (:background ,exu-bg :slant italic))))

 ;; --- SHR ---------------------------------------------------------------------
 ;; TODO: light variant for shr faces
 ;; `(shr-code ((t (:foreground ,exu-red))))
 ;; `(shr-link ((t (:box nil :underline t))))

 ;; --- ORG ---------------------------------------------------------------------
 ;; TODO: light variant for org-verbatim
 ;; `(org-verbatim ((t (:background ,exu-bg :foreground ,exu-blue))))
 `(org-block ((t (:background ,exu-bg-dim2))))
 `(org-block-begin-line ((t (:background ,exu-bg-dim2))))
 `(org-block-end-line ((t (:background ,exu-bg-dim2))))


 ;; ANSI colors
 ;; `(ansi-color-bright-black ((t (:foreground ,exu-fg ))))

 ;; --- OTHER PLUGINS  ----------------------------------------------------------
 `(colorful-base ((t (:box (:line-width (1 . 1) :color ,exu-black :style flat-button)))))

 )

;; =============================================================================
;; TODO: Faces currently defined in config files that should move to theme
;; =============================================================================
;;
;; --- From core/evil-settings.el ----------------------------------------------
;; Evil cursor colors (these are variables, not faces — need a theme-switch hook):
;;   (evil-insert-state-cursor  '(box "#FF7F9F"))   ; pink
;;   (evil-normal-state-cursor  '(box "#FF8020"))   ; orange
;; Light equivalents might be:
;;   (evil-insert-state-cursor  '(box "#990000"))   ; exu-red or similar
;;   (evil-normal-state-cursor  '(box "#0031a9"))   ; exu-blue
;;
;; --- From core/settings.el ---------------------------------------------------
;; Highlight indent guides (set-face-foreground calls):
;;   highlight-indent-guides-stack-character-face  "#D7AF00"
;;   highlight-indent-guides-character-face        "dimgray"
;; These should become theme faces. Light variants could keep similar values
;; since they read well on light bg too.
;;
;; --- From code/lisp-editing.el -----------------------------------------------
;; Already has TODO to move. Custom faces for highlight-parentheses:
;;   (defface my/hl-paren-innermost '((t :background "#5C4800" :foreground "#D7AF00")))
;;   (defface my/hl-paren-middle    '((t :foreground "#d75f00")))
;; Light equivalents:
;;   (defface my/hl-paren-innermost '((t :background "#FFF3C0" :foreground "#7A5900")))
;;   (defface my/hl-paren-middle    '((t :foreground "#A04000")))
;;
;; --- From code/flycheck-setup.el ---------------------------------------------
;; quick-peek-background-face (set-face-attribute to nil bg, inherit default):
;;   Low priority — already inherits default, works across themes.
;;
;; =============================================================================

(provide-theme 'my-modus-mono)
;;; my-modus-mono-theme.el ends here
