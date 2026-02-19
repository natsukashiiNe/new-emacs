;;; my-modus-mono-dark-theme.el --- Monochrome theme -*- lexical-binding: t; -*-

;;; Commentary:
;; (Almost) monochrome dark theme based on the modus-vivendi-tinted

;;; Code:
(defconst exu-black       "#000000")
(defconst exu-white       "#FFFFFF")

(defconst exu-fg          "#DCDCDC")
(defconst exu-bg          "#0d0e1c")
(defconst exu-accent      "#FF8020")
(defconst exu-accent-weak "#FD994D")
(defconst exu-accent-alt  "#ff7f9f")
(defconst exu-accent-alt2 "#FBD05C")
(defconst exu-yellow      "#D7Af00")

(defconst exu-strong      "#f00167")


(defconst exu-bg-red      "#352637")        ;; insert mode CursorLine bg
(defconst exu-magenta     "#b0589e")       ;; insert mode cursor


;;(defconst exu-main-sup1   "#c7a349")
(defconst exu-main-sup1   "#F6C177")
(defconst exu-main-sup2   "#9ACC8D")
(defconst exu-main-sup3   "#c86496")
;;(defconst exu-main-sup3   "#dc6ea5")
(defconst exu-grey        "#AAAAAA")
(defconst exu-grey2       "#989898")
(defconst exu-comment     "#5e5e5e")
(defconst exu-viol        "#a884e0")

(defconst exu-italic      'italic)
(defconst exu-bold        'bold)
;; DIFFERENT BGS
(defconst exu-bg3 "#1d2235")
(defconst exu-bg-sel "#1a1c35")


;; DIFFRENT FGS
(defconst exu-fg2 "#333333")

;; DIAGNOSTICS COLOR
;; -- Error
(defconst exu-cyan-accent     "#50E5FF")
(defconst exu-cyan-dimmer     "#136F80")
(defconst exu-bg-cyan         "#205158")

;; -- Warn
(defconst exu-magenta-accent  "#f78fe7")
(defconst exu-magenta-dimmer  "#b0589e")
(defconst exu-bg-magenta      "#4a1c4a")

;; -- Error
(defconst exu-green-accent     "#60DC60")
(defconst exu-green-dimmer     "#0B5E30")
(defconst exu-bg-green         "#C7EAD9")
;; -- Warn
(defconst exu-orange-accent    "#FF8700")
(defconst exu-orange-light     "#A64700")
(defconst exu-bg-orange        "#FFD0A0")
;; -- Info
(defconst exu-blue-light       "#1565C0") ;; "#1E88E5","#0D47A1"
(defconst exu-bg-blue          "#D7ECFF")

(defconst exu-slate-accent  "#88a8f7")
(defconst exu-slate-dimmer  "#6580a8")
(defconst exu-bg-slate      "#1c2e4a")


;; dynamic
(defface cursor-insert
  '((t (:background "#b0589e")))
  "Cursor face for Evil insert mode."
  :group 'basic-faces)

(defface hl-line-insert
  '((t (:background "#352637" :extend t)))
  "Highlight line face for Evil insert mode."
  :group 'basic-faces)

(defface line-number-current-line-insert
  '((t (:background "#352637")))
  "Current line number face for Evil insert mode."
  :group 'basic-faces)


(deftheme my-modus-mono-dark
  "A minimal and monochrome-inspired override for modus-operandi-tinted.")

;; First, load the base theme:
(load-theme 'modus-vivendi t)

;; Now override syntax and LSP faces:
(custom-theme-set-faces
 'my-modus-mono-dark

 `(cursor  ((t  (:background ,exu-orange-accent))))
 `(hl-line ((t  (:background "#222244" :extend t))))
 `(line-number-current-line ((t (:foreground "#FFFFFF" :background "#870000" :slant normal :weight normal))))
 `(show-paren-match ((t  (:foreground ,exu-black :background ,exu-yellow :extend t))))

 `(cursor-insert  ((t  (:background "#ff8020"))))
 `(hl-line-insert ((t  (:background ,exu-bg-red :extend t))))
 `(line-number-current-line-insert ((t (:foreground "#000000" :background "#ff8020" :slant normal :weight normal))))

 `(default      ((t  (:background unspecified))))
 `(line-number  ((t  (:background unspecified))))
 `(fringe       ((t  (:background unspecified))))

 ;; DEFAULT AND UI
 `(avy-lead-face       ((t  (:foreground ,exu-black :background ,exu-cyan-accent :weight normal))))
 `(avy-lead-face-0       ((t  (:foreground ,exu-black :background ,exu-cyan-accent :weight normal))))
 `(avy-lead-face-1       ((t  (:foreground ,exu-black :background ,exu-cyan-accent :weight normal))))
 `(avy-lead-face-2       ((t  (:foreground ,exu-black :background ,exu-cyan-accent :weight normal))))

 
 `(help-key-binding   ((t  (:foreground ,exu-black  :background ,exu-main-sup1 :weight normal ))))
 `(help-argument-name ((t  (:foreground ,exu-accent :weight bold ))))

 `(button            ((t  (:foreground ,exu-accent-alt :slant italic :weight normal :underline (:color ,exu-accent-alt)))))
 `(minibuffer-prompt ((t  (:foreground ,exu-black  :background ,exu-strong :box nil ))))
 `(border            ((t  (:foreground ,exu-strong :background ,exu-black :box nil ))))
 `(vertico-current   ((t  (:background "#444444" :weight normal :slant normal))))

 `(orderless-match-face-0   ((t  (:foreground ,exu-strong :weight bold :slant normal))))
 `(orderless-match-face-1   ((t  (:foreground ,exu-accent :weight bold :slant normal))))
 `(orderless-match-face-2   ((t  (:foreground ,exu-yellow :weight bold :slant normal))))
 `(orderless-match-face-3   ((t  (:foreground ,exu-accent-alt :weight bold :slant normal))))
 
 `(marginalia-documentation ((t  (:foreground ,exu-main-sup3 :slant ,exu-italic :weight normal))))
 `(marginalia-key           ((t  (:foreground ,exu-yellow    :slant ,exu-italic :weight normal))))
 
 `(tab-bar ((t  (:foreground ,exu-accent :background ,exu-bg3 :box nil ))))
 `(tab-bar-tab ((t  (:foreground ,exu-accent :background ,exu-bg3 :box nil ))))
 `(tab-bar-tab-inactive ((t  (:foreground ,exu-grey2 :background ,exu-bg3 :box nil ))))

 `(font-lock-function-name-face ((t  (:foreground ,exu-accent-alt :slant normal :weight normal))))
 `(font-lock-function-call-face ((t  (:foreground ,exu-accent-alt :slant normal :weight normal))))
 `(font-lock-builtin-face       ((t  (:foreground ,exu-accent-alt :slant normal :weight normal))))
 `(font-lock-type-face          ((t  (:foreground ,exu-accent-weak :slant normal :weight normal))))
 `(font-lock-variable-name-face ((t  (:foreground ,exu-fg :slant normal :weight normal))))
 `(font-lock-variable-use-face ((t  (:foreground ,exu-fg :slant normal :weight normal))))
 `(font-lock-property-name-face ((t  (:foreground ,exu-fg :slant normal :weight normal))))
 `(font-lock-property-use-face ((t  (:foreground ,exu-fg :slant normal :weight normal))))
 `(font-lock-keyword-face       ((t  (:foreground ,exu-accent-weak :slant normal :weight normal))))
 ;;`(font-lock-keyword-face       ((t  (:foreground ,exu-accent-alt2 :slant normal :weight bold))))
 `(font-lock-string-face        ((t  (:foreground ,exu-main-sup1 :slant normal :weight normal))))
 `(font-lock-negation-char-face ((t  (:inherit font-lock-escape-face))))
 `(font-lock-comment-face       ((t  (:foreground ,exu-comment :slant ,exu-italic :weight normal))))
 `(font-lock-doc-face           ((t  (:foreground ,exu-viol :slant ,exu-italic :weight normal))))
 `(font-lock-doc-markup-face    ((t  (:foreground ,exu-main-sup1 :slant normal :weight normal))))
 `(font-lock-constant-face      ((t  (:foreground ,exu-main-sup2 :slant normal :weight normal))))
 `(font-lock-number-face        ((t  (:foreground ,exu-main-sup2 :slant ,exu-italic :weight normal))))


 ;; LSP faces
 ;; Errors, warnings, etc. can be minimal or softly highlighted
 `(lsp-face-highlight-textual ((t
				(:background unspecified
					     :foreground unspecified
					     :box (:line-width 1 :color ,exu-accent :style released-button)))))

 `(lsp-face-highlight-read ((t
			     (:background unspecified
					  :foreground unspecified
					  :box (:line-width 1 :color ,exu-accent :style released-button)))))

 `(lsp-face-highlight-write ((t
			      (:background unspecified
					   :foreground unspecified
					   :weight bold
					   :box (:line-width 1 :color ,exu-accent :style released-button)))))

 `(lsp-face-error       ((t  (:background ,exu-bg-cyan :underline nil ))))
 `(lsp-face-warning     ((t  (:background ,exu-bg-magenta :underline nil :weight ,exu-bold))))
 `(lsp-face-info        ((t  (:background ,exu-bg-sel :underline nil ))))
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
 
 `(success     ((t (:background ,exu-black        :foreground ,exu-main-sup3 :weight bold))))
 
 `(error     ((t (:foreground ,exu-bg-cyan        :background ,exu-cyan-accent :extend t))))
 `(warning   ((t (:foreground ,exu-magenta-accent :background ,exu-bg-magenta :weight ,exu-bold :extend t))))
 `(info             ((t (:background ,exu-bg-sel     :underline nil ))))

 ;; diagnostics
 ;;`(flycheck-error            ((t (:background ,exu-bg-cyan    :underline nil ))))
 ;;`(flycheck-warning          ((t (:background ,exu-bg-magenta :underline nil :weight ,exu-bold))))
 ;;`(flycheck-info             ((t (:background ,exu-bg-sel     :underline nil ))))
 `(flycheck-error            ((t (:underline t ))))
 `(flycheck-warning          ((t (:underline t ))))
 `(flycheck-info             ((t (:underline t ))))

 ;; inline
 `(flycheck-inline-error     ((t (:foreground ,exu-cyan-accent    :background ,exu-bg-cyan :extend t))))
 `(flycheck-inline-warning   ((t (:foreground ,exu-magenta-accent :background ,exu-bg-magenta :weight ,exu-bold :extend t))))
 `(flycheck-inline-info      ((t (:foreground ,exu-slate-accent   :background ,exu-bg-slate :extend t ))))

 ;; icons
 `(flycheck-fringe-error     ((t (:foreground ,exu-bg-cyan    :background ,exu-bg-orange))))
 `(flycheck-fringe-warning   ((t (:foreground ,exu-bg-magenta :background ,exu-bg-orange))))
 `(flycheck-fringe-info      ((t (:foreground ,exu-blue-light     :background ,exu-bg-orange))))

 `(flyover-error     ((t (:foreground ,exu-cyan-accent    :background ,exu-bg-cyan :extend t))))
 `(flyover-warning   ((t (:foreground ,exu-magenta-accent :background ,exu-bg-magenta  :extend t))))
 `(flyover-info      ((t (:foreground ,exu-slate-accent   :background ,exu-bg-slate :extend t ))))

 `(flycheck-overlay-marker             ((t (:foreground ,exu-cyan-accent :background ,exu-bg-orange))))


 ;; --- GIT ----------------------------------------------------------------
 `(magit-diff-file-heading     ((t (:foreground ,exu-accent-weak :weight normal))))
 `(magit-filename              ((t (:foreground ,exu-accent-alt2 :weight normal))))
 `(magit-hash                  ((t (:foreground ,exu-accent-alt  :weight normal))))
 `(magit-branch-local          ((t (:foreground ,exu-strong      :weight normal))))
 `(magit-branch-remote         ((t (:foreground ,exu-strong      :weight normal))))


 `(diff-hl-insert ((t (:foreground "#88f5b3" :background ,(face-background 'default)))))
 `(diff-hl-change ((t (:foreground "#a0c4ff" :background ,(face-background 'default)))))
 `(diff-hl-delete ((t (:foreground "#ff8fa3" :background ,(face-background 'default)))))

 ;; --- Dired ----------------------------------------------------------------
 `(dirvish-hl-line             ((t (:foreground ,exu-black :background ,exu-accent :weight normal))))
 `(dired-directory             ((t (:foreground ,exu-accent-weak :weight normal))))
 `(dirvish-file-size           ((t (:foreground ,exu-grey2 :weight normal))))

 ;; --- Compilaton -----------------------------------------------------------

 `(comint-highlight-prompt     ((t (:foreground ,exu-black :background ,exu-accent-alt  :inherit nil))))



 
 ;; --- Treemacs ----------------------------------------------------------------
 ;; `(treemacs-git-untracked-face    ((t (:foreground ,exu-strong  :weight normal))))
 ;; `(treemacs-git-modified-face     ((t (:foreground ,exu-strong  :weight normal))))

 ;; `(treemacs-nerd-icons-root-face  ((t (:foreground ,exu-strong  :weight normal))))
 ;; `(treemacs-directory-face        ((t (:foreground ,exu-strong  :weight normal))))
 ;; `(treemacs-root-face             ((t (:foreground ,exu-strong  :weight normal))))


 ;; --- HELP --------------------------------------------------------------------
 ;; `(help-argument-name             ((t (:foreground ,exu-strong  :weight normal))))








 ;; --- TELEGA ------------------------------------------------------------------

 `(telega-msg-heading ((t (:background ,exu-bg :weight bold))))
 `(telega-msg-inline-forward ((t (:background ,exu-bg :slant italic))))

 ;; --- ORG ---------------------------------------------------------------------
 `(org-verbatim ((t (:background ,exu-black :foreground ,exu-slate-accent))))
 ;;`(org-property-value ((t (:background ,exu-black :foreground ,exu-slate-accent))))


 )
;; --- EVIL MODE CURSOR/LINE DYNAMIC COLORS ------------------------------------

(defvar-local my-evil-face-remappings nil
  "Alist of (STATE . (REMAP-COOKIES...)) for Evil state face changes.")

(defun my-evil-apply-face-remaps (states face-mapping-alist)
  "Apply face remappings when entering Evil STATES.
STATES is a list of Evil state symbols (insert, visual, normal, etc.).
FACE-MAPPING-ALIST is an alist of (TARGET-FACE . SOURCE-FACE) pairs.

For each pair, TARGET-FACE will be remapped to look like SOURCE-FACE.

Example:
  (my-evil-apply-face-remaps \='(insert)
    \='((cursor . cursor-insert)
      (hl-line . hl-line-insert)
      (line-number-current-line . line-number-current-line-insert)))"
  
  (dolist (state states)
    (let ((entry-hook (intern (format "evil-%s-state-entry-hook" state)))
          (exit-hook (intern (format "evil-%s-state-exit-hook" state))))
      
      ;; Entry hook: Apply remappings
      (add-hook entry-hook
                (lambda ()
                  (let ((remaps nil))
                    (dolist (mapping face-mapping-alist)
                      (let* ((target-face (car mapping))
                             (source-face (cdr mapping))
                             ;; Get all attributes from source face
                             (fg (face-attribute source-face :foreground nil 'default))
                             (bg (face-attribute source-face :background nil 'default))
                             (weight (face-attribute source-face :weight nil 'default))
                             (slant (face-attribute source-face :slant nil 'default))
                             (remap-spec nil))
                        
                        ;; Build remap spec only for specified attributes
                        (unless (eq fg 'unspecified)
                          (push :foreground remap-spec)
                          (push fg remap-spec))
                        (unless (eq bg 'unspecified)
                          (push :background remap-spec)
                          (push bg remap-spec))
                        (unless (eq weight 'unspecified)
                          (push :weight remap-spec)
                          (push weight remap-spec))
                        (unless (eq slant 'unspecified)
                          (push :slant remap-spec)
                          (push slant remap-spec))
                        
                        ;; Apply remap if we have any attributes
                        (when remap-spec
                          (push (apply #'face-remap-add-relative target-face (nreverse remap-spec))
                                remaps))))
                    
                    ;; Store remaps for this state
                    (push (cons state remaps) my-evil-face-remappings)
                    
                    ;; Force refresh hl-line
                    (when (bound-and-true-p hl-line-mode)
                      (hl-line-highlight)))))
      
      ;; Exit hook: Remove remappings
      (add-hook exit-hook
                (lambda ()
                  (when-let* ((remaps (alist-get state my-evil-face-remappings)))
                    (dolist (cookie remaps)
                      (face-remap-remove-relative cookie))
                    (setq my-evil-face-remappings
                          (assq-delete-all state my-evil-face-remappings))
                    
                    ;; Force refresh hl-line
                    (when (bound-and-true-p hl-line-mode)
                      (hl-line-highlight))))))))

;; Usage in your theme file:
(with-eval-after-load 'evil
  (my-evil-apply-face-remaps
   '(insert)  ; States to apply to
   '((cursor . cursor-insert)
     (hl-line . hl-line-insert)
     (line-number-current-line . line-number-current-line-insert)))
  
  ;; You could also add visual mode with different colors:
  ;; (my-evil-apply-face-remaps
  ;;  '(visual)
  ;;  '((cursor . cursor-visual)
  ;;    (hl-line . hl-line-visual)))
  )



(provide-theme 'my-modus-mono-dark)
;;; my-modus-mono-dark-theme.el ends here
