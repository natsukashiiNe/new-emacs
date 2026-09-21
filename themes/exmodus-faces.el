;;; exmodus-faces.el --- Palettes and faces of the exmodus themes -*- lexical-binding: t; -*-

;;; Commentary:
;; Single source of truth for exmodus-dark and exmodus-light.
;; Merged from my-modus-mono-dark-theme.el and my-modus-mono-theme.el.
;;
;; How to read a spec:
;;   (face-name
;;    :extend t                        ; common to both variants
;;    :dark  (:foreground err-fg ...)  ; err-fg comes from the DARK palette
;;    :light (:foreground err-fg ...)) ; err-fg comes from the LIGHT palette
;;
;; Attributes inside :dark/:light override common ones with the same key,
;; nil included: `:dark (... :extend nil)' beats a common `:extend t'.
;; Nested values (:underline / :box plists) are replaced whole, not merged.
;;
;; `:dark todo' / `:light (todo "hint")' -- the variant is not chosen yet.
;; M-x my-theme/report-todos shows the whole list.
;;
;; Usage:
;;   (require 'exmodus-faces)
;;   (my-theme/load "exmodus" 'dark)   ; or M-x my-theme/toggle

;;; Code:

(require 'exmodus-theme-gen)

;; =============================================================================
;; BASE THEMES
;; =============================================================================

(setq my-theme/base-themes
      '((dark  . modus-vivendi)
        (light . modus-operandi-tinted)))

;; =============================================================================
;; PALETTES  (two fully independent ones; names need not match)
;; =============================================================================

(setq my-theme/palettes
      '((dark
         ;; -- base --------------------------------------------------------
         (black          "#000000")
         (white          "#FFFFFF")
         (fg             "#DCDCDC")
         (fg2            "#333333")
         (bg             "#0d0e1c")
         (bg3            "#1d2235")
         (bg-sel         "#1a1c35")
         (bg-red         "#352637")
         (hl-line-bg     "#222244")
         (sel-bg         "#444444")
         (grey           "#AAAAAA")
         (grey2          "#989898")
         (comment        "#5e5e5e")
         ;; -- accents -----------------------------------------------------
         (accent         "#FF8020")
         (accent-weak    "#FD994D")
         (accent-alt     "#e86671")
         (accent-alt2    "#FBD05C")
         (yellow         "#D7AF00")
         (strong         "#f00167")
         (magenta        "#b0589e")
         (viol           "#a884e0")
         (main-sup1      "#F6C177")
         (main-sup2      "#9ACC8D")
         (main-sup3      "#c86496")
         ;; -- diagnostics -------------------------------------------------
         (cyan-accent    "#50E5FF")
         (cyan-dimmer    "#136F80")
         (bg-cyan        "#205158")
         (magenta-accent "#f78fe7")
         (magenta-dimmer "#b0589e")
         (bg-magenta     "#4a1c4a")
         (slate-accent   "#88a8f7")
         (slate-dimmer   "#6580a8")
         (bg-slate       "#1c2e4a")
         (green-accent   "#60DC60")
         (green-dimmer   "#0B5E30")
         (bg-green       "#C7EAD9")
         (orange-accent  "#FF8700")
         (orange-light   "#A64700")
         (bg-orange      "#FFD0A0")
         (blue-light     "#1565C0")
         (bg-blue        "#D7ECFF")
         ;; -- extracted from literals -------------------------------------
         (cur-line-fg    white)
         (cur-line-bg    "#870000")
         (insert-cursor  "#ff8020")
         (eol-err        "#4AB9C9")
         (eol-warn       "#B77EC4")
         (eol-note       "#3F7CAA")
         (diff-add       "#88f5b3")
         (diff-mod       "#a0c4ff")
         (diff-del       "#ff8fa3")
         (link-brown     "#C58671")
         ;; -- semantic aliases --------------------------------------------
         (err-fg         cyan-accent)
         (err-bg         bg-cyan)
         (warn-fg        magenta-accent)
         (warn-bg        bg-magenta)
         (info-fg        slate-accent)
         (info-bg        bg-slate))

        (light
         ;; -- base --------------------------------------------------------
         (black          "#000000")
         (white          "#FFFFFF")
         (fg             "#000000")
         (fg2            "#333333")
         (bg             "#EBDDB1")
         (bg-dim1        "#E9E5BB")
         (bg-dim2        "#DBCC97")
         (bg3            "#CAB9B2")
         (bg-pink        "#F5B0B0")
         (grey           "#5e5e5e")
         (comment        "#5e5e5e")
         ;; -- accents -----------------------------------------------------
         (red            "#990000")
         (lblue          "#304483")
         (blue           "#0031a9")
         (blue-accent    "#0000FF")
         (purple-dimmer  "#8E24AA")
         (purple-bg      "#F0EEFF")
         ;; -- diagnostics -------------------------------------------------
         (green-accent   "#60DC60")
         (green-dimmer   "#0B5E30")
         (bg-green       "#C7EAD9")
         (orange-accent  "#FF8700")
         (orange-light   "#A64700")
         (orange-dimmer  "#A0422E")
         (bg-orange      "#FFD0A0")
         (blue-light     "#1565C0")
         (bg-blue        "#D7ECFF")
         ;; -- extracted from literals -------------------------------------
         (cur-line-fg    white)
         (cur-line-bg    "#870000")
         (violet-ink     "#4020CC")
         (symbol-bg      "#f6c3cf")
         (diff-add       "#37712F")
         (diff-mod       "#485496")
         (diff-del       "#914834")
         ;; -- semantic aliases --------------------------------------------
         ;; NB: in the light theme "error" is historically green -- as in the original.
         (err-fg         green-dimmer)
         (err-bg         bg-green)
         (warn-fg        orange-light)
         (warn-bg        bg-orange)
         (info-fg        blue-light)
         (info-bg        bg-blue))))

;; =============================================================================
;; FACES
;; =============================================================================

(my-theme/define-themes
 "exmodus"

 (diagnostics/inline-error
  :extend t
  :dark  (:foreground err-fg  :background err-bg)
  :light (:foreground err-fg  :background err-bg :weight bold))
 (diagnostics/inline-warn
  :extend t
  :dark  (:foreground warn-fg :background warn-bg)
  :light (:foreground warn-fg :background warn-bg))
 (diagnostics/inline-info
  :extend t
  :dark  (:foreground info-fg :background info-bg)
  :light (:foreground info-fg :background info-bg))

 (diagnostics/fringe-error
  :dark  (:foreground err-fg  :background err-bg)
  :light (:foreground green-accent  :background bg-orange))
 (diagnostics/fringe-warn
  :dark  (:foreground warn-fg :background warn-bg :weight bold)
  :light (:foreground orange-accent :background bg-orange))
 (diagnostics/fringe-info
  :dark  (:foreground info-fg :background info-bg)
  :light (:foreground blue-light    :background bg-orange))

 ;; ===========================================================================
 ;; CORE
 ;; ===========================================================================

 (default
  :dark  (:background black)
  :light (:background bg))

 (cursor
  :dark  (:background orange-accent)
  :light (:background red))

 (hl-line
  :extend t
  :dark  (:background hl-line-bg)
  :light (:background bg-dim2))

 (line-number
  :dark  (:background black)
  :light (:background bg-dim2))          ; TODO: pick a light shade

 (line-number-current-line
  :slant normal :weight normal
  :dark  (:foreground cur-line-fg :background cur-line-bg)
  :light (:foreground cur-line-fg :background cur-line-bg))

 (fringe
  :dark  (:background black)
  :light (:background bg-dim2))          ; TODO: pick a light shade

 (margin
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:background bg-dim2))

 (show-paren-match
  :weight bold
  :dark  (:background bg-slate :foreground slate-accent :underline t)
  :light (:foreground green-dimmer :background bg-green :extend t))

 ;; -- evil states (dynamic, see my-evil-apply-face-remaps) -------------------
 (cursor-insert
  :dark  (:background insert-cursor)
  :light (todo "the light theme only had hl-line-insert; cursor not set"))

 (hl-line-insert
  :extend t
  :dark  (:background bg-red)
  :light (:background bg-pink))

 (line-number-current-line-insert
  :slant normal :weight normal
  :dark  (:foreground black :background insert-cursor)
  :light (todo "missing in the light theme"))

 ;; -- lisp / parens ---------------------------------------------------------
 (my/hl-paren-middle
  :dark  (todo "from lisp-editing.el: :foreground \"#d75f00\"")
  :light (:foreground blue-light :background bg-blue))

 (my/hl-paren-innermost
  :weight bold
  :dark  (todo "from lisp-editing.el: :background \"#5C4800\" :foreground \"#D7AF00\"")
  :light (:background purple-dimmer :foreground bg-pink))

 (hl-sexp-face
  :dark  (todo "from lisp-editing.el: :background \"#1c1c1c\"")
  :light (:background bg-dim2))

 (hi
  :weight bold :slant normal
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:foreground blue-light :background bg-blue))

 ;; ===========================================================================
 ;; UI
 ;; ===========================================================================

 (avy-lead-face
  :weight normal
  :dark  (:foreground black :background cyan-accent)
  :light (:foreground bg :background red))
 (avy-lead-face-0
  :weight normal
  :dark  (:foreground black :background cyan-accent)
  :light (:foreground bg :background red))
 (avy-lead-face-1
  :weight normal
  :dark  (:foreground black :background cyan-accent)
  :light (:foreground bg :background red))
 (avy-lead-face-2
  :weight normal
  :dark  (:foreground black :background cyan-accent)
  :light (:foreground bg :background red))

 (button
  :slant italic
  :dark  (:foreground accent-alt :weight normal :underline (:color accent-alt))
  :light (:foreground purple-dimmer :background purple-bg :weight bold
		      :underline (:color purple-dimmer)))

 (custom-link
  :dark  (:foreground main-sup2 :underline t)
  :light todo)                           ; TODO: missing in the light theme

 (minibuffer-prompt
  :box nil
  :dark  (:foreground black :background strong)
  :light (:foreground bg    :background red))

 (border
  :box nil
  :dark  (:foreground strong :background black)
  :light (:foreground red    :background bg))

 (internal-border
  :dark  (:foreground accent)
  :light (:foreground red))

 (vertico-current
  :weight normal :slant normal
  :dark  (:background sel-bg)
  :light (:background bg-pink))

 (orderless-match-face-0
  :weight bold :slant normal
  :dark  (:foreground strong)
  :light (:foreground blue-light :background bg-blue))
 (orderless-match-face-1
  :weight bold :slant normal
  :dark  (:foreground accent)
  :light (:foreground green-dimmer :background bg-green))
 (orderless-match-face-2
  :weight bold :slant normal
  :dark  (:foreground yellow)
  :light (:foreground purple-dimmer :background purple-bg))
 (orderless-match-face-3
  :weight bold :slant normal
  :dark  (:foreground accent-alt)
  :light (:foreground orange-dimmer :background bg-orange))

 (marginalia-documentation
  :slant italic :weight normal
  :dark  (:foreground main-sup3)
  :light (:foreground blue))
 (marginalia-key
  :slant italic :weight normal
  :dark  (:foreground yellow)
  :light (:foreground red))

 (consult-key
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:foreground purple-dimmer :background purple-bg
		      :slant italic :weight bold
		      :underline (:color purple-dimmer)))

 (read-multiple-choice-face
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:foreground purple-dimmer :background purple-bg
		      :slant italic :weight bold
		      :underline (:color purple-dimmer)))

 ;; -- tab-bar ---------------------------------------------------------------
 (tab-bar
  :weight normal
  :dark  (:foreground accent :background bg3 :box nil)
  :light (:foreground fg2 :background bg3
		      :box (:line-width (-1 . 1) :color black :style flat-button)))
 (tab-bar-tab
  :dark  (:foreground accent :background bg3 :box nil)
  :light (:foreground red :background bg3 :weight bold
		      :box (:line-width (-1 . 1) :color black :style flat-button)))
 (tab-bar-tab-inactive
  :weight normal
  :dark  (:foreground grey2 :background bg3 :box nil)
  :light (:foreground fg2 :background bg3
		      :box (:line-width (-1 . 1) :color black :style flat-button)))

 ;; ===========================================================================
 ;; FONT-LOCK
 ;; ===========================================================================

 (font-lock-function-name-face
  :dark  (:foreground accent-alt :slant normal :weight normal)
  :light (:foreground fg :slant italic))
 (font-lock-function-call-face
  :dark  (:foreground accent-alt :slant normal :weight normal)
  :light (:foreground fg :slant italic))
 (font-lock-builtin-face
  :dark  (:foreground accent-alt :slant normal :weight normal)
  :light (:foreground blue :slant italic))
 (font-lock-type-face
  :dark  (:foreground accent-weak :slant normal :weight normal)
  :light (:foreground fg :weight bold))
 (font-lock-variable-name-face
  :dark  (:foreground fg :slant normal :weight normal)
  :light (:foreground fg))
 (font-lock-variable-use-face
  :dark  (:foreground fg :slant normal :weight normal)
  :light (:foreground fg))
 (font-lock-property-name-face
  :dark  (:foreground fg :slant normal :weight normal)
  :light (:foreground fg))
 (font-lock-property-use-face
  :dark  (:foreground fg :slant normal :weight normal)
  :light (:foreground fg))
 (font-lock-keyword-face
  :dark  (:foreground accent-weak :slant normal :weight normal)
  :light (:foreground red :weight bold))
 (font-lock-string-face
  :dark  (:foreground main-sup1 :slant normal :weight normal)
  :light (:foreground red :slant italic))
 (font-lock-negation-char-face
  :inherit font-lock-escape-face)
 (font-lock-comment-face
  :slant italic :weight normal
  :dark  (:foreground comment)
  :light (:foreground comment))
 (font-lock-doc-face
  :slant italic :weight normal
  :dark  (:foreground slate-dimmer)
  :light (:foreground lblue))            ; TODO: pick a light shade
 (font-lock-doc-markup-face
  :dark  (:foreground main-sup1 :slant normal :weight normal)
  :light (:foreground violet-ink :slant normal :extend t))
 (font-lock-constant-face
  :dark  (:foreground main-sup2 :slant normal :weight normal)
  :light (:foreground fg :weight bold))
 (font-lock-number-face
  :slant italic
  :dark  (:foreground main-sup2 :weight normal)
  :light (:foreground blue))

 ;; ===========================================================================
 ;; LSP
 ;; ===========================================================================

 (symbol-overlay-default-face
  :foreground unspecified
  :dark  (:background fg2 :weight bold :box nil)
  :light (:background symbol-bg :weight normal
		      :box (:line-width (2 . -2) :color black :style flat-button)))

 ;; In both originals these three were later overridden with `unspecified' --
 ;; the duplicate is kept in its final form.
 (lsp-face-highlight-textual :background unspecified)
 (lsp-face-highlight-read    :background unspecified)
 (lsp-face-highlight-write   :background unspecified)

 (lsp-face-error
  :dark  (:background bg-cyan :underline nil)
  :light (:foreground red :background unspecified :weight bold))
 (lsp-face-warning
  :dark  (:background bg-magenta :underline nil :weight bold)
  :light (:foreground red :background unspecified :slant italic))
 (lsp-face-info
  :dark  (:background bg-sel :underline nil)
  :light (:foreground grey :slant italic))
 (lsp-face-hint
  :slant italic
  :dark  (:foreground grey)
  :light (:foreground grey))

 ;; -- semantic tokens: identical in both variants ---------------------------
 (lsp-face-semhl-function  :inherit font-lock-function-name-face)
 (lsp-face-semhl-member    :inherit font-lock-function-name-face)
 (lsp-face-semhl-variable  :inherit font-lock-variable-name-face)
 (lsp-face-semhl-type      :inherit font-lock-type-face)
 (lsp-face-semhl-parameter
  :slant italic
  :dark  (:foreground fg)
  :light (:foreground fg))
 (lsp-face-semhl-operator  :inherit font-lock-keyword-face)
 (lsp-face-semhl-string    :inherit font-lock-string-face)
 (lsp-face-semhl-comment   :inherit font-lock-comment-face)
 (lsp-face-semhl-namespace :inherit font-lock-type-face)
 (lsp-face-semhl-constant  :inherit font-lock-constant-face)
 (tree-sitter-hl-face:method.call :inherit font-lock-function-name-face)

 ;; ===========================================================================
 ;; BASE STATES
 ;; ===========================================================================

 (success
  :weight bold
  :dark  (:background black :foreground main-sup3)
  :light (:foreground purple-dimmer :background purple-bg))
 (error
  :extend t
  :dark  (:foreground bg-cyan :background cyan-accent)
  :light (:foreground green-dimmer :background bg-green :weight bold))
 (warning
  :extend t :weight bold
  :dark  (:foreground magenta-accent :background bg-magenta)
  :light (:foreground orange-light :background bg-orange))
 (info
  :dark  (:background bg-sel :underline nil)
  :light (:foreground blue-light :background bg-blue))

 ;; ===========================================================================
 ;; FLYMAKE   (was dark-only)
 ;; ===========================================================================

 (flymake-error
  :dark  (:underline (:color err-fg  :style wave :position -4))
  :light (:underline (:color err-fg  :style wave :position -4)))
 (flymake-warning
  :dark  (:underline (:color warn-fg :style wave :position -4))
  :light (:underline (:color warn-fg :style wave :position -4)))
 (flymake-note
  :dark  (:underline (:color info-fg :style wave :position -4))
  :light (:underline (:color info-fg :style wave :position -4)))

 (flymake-error-echo
  :weight bold
  :dark  (:foreground err-fg)
  :light (:foreground err-fg))
 (flymake-warning-echo
  :weight bold
  :dark  (:foreground warn-fg)
  :light (:foreground warn-fg))
 (flymake-note-echo
  :weight bold
  :dark  (:foreground info-fg)
  :light (:foreground info-fg))

 (flymake-error-fringe   :inherit diagnostics/fringe-error)
 (flymake-warning-fringe :inherit diagnostics/fringe-warn)
 (flymake-note-fringe    :inherit diagnostics/fringe-info)

 (flymake-error-echo-at-eol
  :height 0.90 :slant italic
  :dark  (:foreground eol-err
		      :box (:line-width (1 . -1) :color eol-err :style flat-button))
  :light (todo "EOL box colors were only picked for the dark theme"))
 (flymake-warning-echo-at-eol
  :height 0.90 :slant italic
  :dark  (:foreground eol-warn
		      :box (:line-width (1 . -1) :color eol-warn :style flat-button))
  :light (todo "EOL box colors were only picked for the dark theme"))
 (flymake-note-echo-at-eol
  :height 0.90 :slant italic
  :dark  (:foreground eol-note
		      :box (:line-width (1 . -1) :color eol-note :style flat-button))
  :light (todo "EOL box colors were only picked for the dark theme"))

 (flycheck-error-list-highlight
  :weight bold :extend t
  :dark  (:box (:line-width (2 . 2) :color white :style flat-button))
  :light (todo "missing in the light theme"))

 ;; ===========================================================================
 ;; FLYCHECK / FLYOVER   (was light-only; now goes through the semantic layer)
 ;; ===========================================================================

 (flycheck-error
  :underline nil :weight bold
  :dark  (:background err-bg)
  :light (:background err-bg))
 (flycheck-warning
  :underline nil
  :dark  (:background warn-bg)
  :light (:background warn-bg))
 (flycheck-info
  :underline nil
  :dark  (:background info-bg)
  :light (:background info-bg))

 (flycheck-inline-error   :inherit diagnostics/inline-error)
 (flycheck-inline-warning :inherit diagnostics/inline-warn)
 (flycheck-inline-info    :inherit diagnostics/inline-info)

 (flycheck-fringe-error   :inherit diagnostics/fringe-error)
 (flycheck-fringe-warning :inherit diagnostics/fringe-warn)
 (flycheck-fringe-info    :inherit diagnostics/fringe-info)

 (flyover-error   :inherit diagnostics/inline-error)
 (flyover-warning :inherit diagnostics/inline-warn)
 (flyover-info    :inherit diagnostics/inline-info)

 ;; ===========================================================================
 ;; GIT
 ;; ===========================================================================

 (magit-diff-file-heading
  :weight normal
  :dark  (:foreground accent-weak)
  :light (todo "commented out in the light theme: exu-blue"))
 (magit-filename
  :weight normal
  :dark  (:foreground accent-alt2)
  :light (todo "commented out in the light theme: exu-red"))
 (magit-hash
  :weight normal
  :dark  (:foreground accent-alt)
  :light (todo "commented out in the light theme: exu-grey"))
 (magit-branch-local
  :weight normal
  :dark  (:foreground strong)
  :light (todo "commented out in the light theme: exu-red"))
 (magit-branch-remote
  :weight normal
  :dark  (:foreground strong)
  :light (todo "commented out in the light theme: exu-red"))

 ;; The dark original called (face-background 'default) at load time --
 ;; replaced with an explicit background color.
 (diff-hl-insert
  :stipple nil
  :dark  (:foreground diff-add :background black)
  :light (:foreground diff-add :background bg-dim2))
 (diff-hl-change
  :stipple nil
  :dark  (:foreground diff-mod :background black)
  :light (:foreground diff-mod :background bg-dim2))
 (diff-hl-delete
  :stipple nil
  :dark  (:foreground diff-del :background black)
  :light (:foreground diff-del :background bg-dim2))

 ;; ===========================================================================
 ;; DIRED / DIRVISH
 ;; ===========================================================================

 (dired-directory
  :dark  (:foreground accent-weak :weight normal)
  :light (:foreground black :weight bold))
 (dirvish-hl-line
  :weight normal
  :dark  (:foreground black :background accent)
  :light (todo "commented out in the light theme: fg=bg, bg=red"))
 (dirvish-file-size
  :weight normal
  :dark  (:foreground grey2)
  :light (todo "commented out in the light theme: exu-grey"))

 ;; ===========================================================================
 ;; COMPILATION
 ;; ===========================================================================

 (comint-highlight-prompt
  :inherit nil
  :dark  (:foreground black :background accent-alt)
  :light (todo "commented out in the light theme: fg=bg, bg=blue"))

 (compilation-info
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:foreground fg))
 (compilation-line-number
  :dark  todo
  :light (:foreground fg))
 (compilation-column-number
  :dark  todo
  :light (:foreground fg))

 (fancy-compilation-default-face
  :dark  todo
  :light (:foreground fg))
 (fancy-compilation-column-number-face
  :inherit nil
  :dark  todo
  :light (:foreground bg :background blue))
 (fancy-compilation-function-name-face
  :inherit nil
  :dark  todo
  :light (:foreground purple-dimmer))
 (fancy-compilation-info-face
  :dark  todo
  :light (:foreground blue-accent :weight bold :underline nil))
 (fancy-compilation-line-number-face
  :weight bold
  :dark  todo
  :light (:foreground fg))
 (fancy-compilation-warning-face :inherit warning)
 (fancy-compilation-error-face   :inherit error)

 ;; ===========================================================================
 ;; MODELINE
 ;; ===========================================================================

 (doom-modeline-buffer-modified
  :weight bold
  :dark  (:background orange-light :foreground bg-orange)
  :light (:foreground blue-light :background bg-blue
		      :box (:line-width (4 . -4) :color black :style flat-button)))
 (doom-modeline-evil-insert-state
  :dark  (:background orange-light :foreground bg-orange)
  :light todo)                           ; TODO: missing in the light theme
 (mode-line-inactive
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:background bg :foreground unspecified
		      :box (:line-width (-1 . 1) :color black :style flat-button)))

 ;; ===========================================================================
 ;; HELP / CUSTOM
 ;; ===========================================================================

 (help-key-binding
  :dark  (:foreground black :background main-sup1 :weight normal)
  :light (:foreground purple-dimmer :background purple-bg
		      :slant italic :weight bold
		      :underline (:color purple-dimmer)))
 (help-argument-name
  :weight bold
  :dark  (:foreground accent)
  :light (todo "commented out in the light theme: exu-red"))

 (custom-variable-tag
  :weight normal
  :dark  (:foreground accent-weak)
  :light todo)                           ; TODO: missing in the light theme
 (custom-group-tag
  :weight normal
  :dark  (:foreground strong)
  :light todo)
 (custom-state
  :slant italic
  :dark  (:foreground main-sup1)
  :light todo)
 (custom-visibility
  :inherit custom-link :height 0.8
  :dark  nil
  :light todo)

 ;; ===========================================================================
 ;; TELEGA
 ;; ===========================================================================

 (telega-msg-heading
  :weight bold
  :dark  (:background bg)
  :light (:background bg))
 (telega-msg-inline-forward
  :slant italic
  :dark  (:background bg)
  :light (:background bg))

 ;; ===========================================================================
 ;; SHR / INFO
 ;; ===========================================================================

 (shr-code
  :dark  (:foreground accent-weak)
  :light (todo "commented out in the light theme: exu-red"))
 (shr-link
  :box nil :underline t
  :dark  nil
  :light (todo "commented out in the light theme"))
 (shrface-verbatim
  :slant italic
  :dark  (:foreground main-sup1)
  :light todo)                           ; TODO: missing in the light theme
 (Info-quoted
  :dark  (:foreground main-sup1)
  :light todo)

 ;; ===========================================================================
 ;; ORG
 ;; ===========================================================================

 (modus-themes-heading-1
  :extend t :height 1.20 :weight extra-bold
  :dark  (:foreground white)
  :light todo)                           ; TODO: missing in the light theme
 (modus-themes-heading-2
  :extend t :height 1.15 :weight bold
  :dark  (:foreground accent-weak)
  :light todo)
 (modus-themes-heading-3
  :extend t :height 1.1 :weight bold
  :dark  (:foreground main-sup1)
  :light todo)

 (org-verbatim
  :dark  (:background black :foreground slate-accent)
  :light (:background bg :foreground blue))
 (org-link
  :underline t
  :dark  (:foreground link-brown)
  :light todo)                           ; TODO: missing in the light theme

 (org-table
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:background bg :foreground fg))
 (org-level-1
  :weight bold :extend t
  :dark  todo
  :light (:foreground bg :background red))
 (org-indent
  :dark  todo
  :light (:foreground comment))
 (org-hide
  :dark  todo
  :light (:foreground comment))
 (org-block
  :dark  todo
  :light (:background bg-dim2))
 (org-block-begin-line
  :dark  todo
  :light (:background bg-dim2))
 (org-block-end-line
  :dark  todo
  :light (:background bg-dim2))

 ;; ===========================================================================
 ;; MISC
 ;; ===========================================================================

 (colorful-base
  :dark  todo                            ; TODO: missing in the dark theme
  :light (:box (:line-width (1 . 1) :color black :style flat-button))))

;; =============================================================================
;; NON-FACES: theme-dependent variables
;; =============================================================================
;; Evil cursors, flyover tints, highlight-parentheses-colors etc. are
;; strings/symbols, not faces, so they are set from a hook after the theme is
;; enabled, taking values DIRECTLY FROM THE PALETTE (not via face-attribute --
;; that would make a circular dependency theme -> face -> variable -> theme).

(defun exmodus/apply-non-face-vars ()
  "Set variables that depend on the current theme variant."
  (let ((v (or my-theme/current 'dark)))
    (with-eval-after-load 'evil
      (setq evil-insert-state-cursor
            (list 'box (my-theme/color (if (eq v 'dark) 'insert-cursor 'red) v))
            evil-normal-state-cursor
            (list 'box (my-theme/color (if (eq v 'dark) 'accent 'blue) v))))
    (with-eval-after-load 'highlight-parentheses
      (setq highlight-parentheses-colors nil))))

(add-hook 'my-theme/after-load-hook #'exmodus/apply-non-face-vars)

;; =============================================================================
;; REMAINING MOVES FROM THE CONFIG  (non-faces or not migrated yet)
;; =============================================================================
;; core/settings.el:
;;   highlight-indent-guides-stack-character-face  "#D7AF00"  (= yellow, dark)
;;   highlight-indent-guides-character-face        "dimgray"
;;   -> set via set-face-foreground; could become faces here.
;;
;; code/flycheck-setup.el:
;;   quick-peek-background-face -- inherits default, needs no theme entry.
;;
;; code/lisp-editing.el:
;;   defface my/hl-paren-innermost / my/hl-paren-middle -- hardcoded;
;;   after moving them here, remove the set-face-attribute calls there.

(provide 'exmodus-faces)
;;; exmodus-faces.el ends here
