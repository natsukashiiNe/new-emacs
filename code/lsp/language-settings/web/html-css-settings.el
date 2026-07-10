;;; html-css-settings.el --- Settings for HTML / CSS / SCSS. -*- lexical-binding: t; -*-

;;; Commentary:
;; Web markup + styles:
;;   - html-ts-mode for .html / .htm
;;   - css-ts-mode for .css / .scss
;;   - emmet-mode expansion in markup and css regions
;;   - the vscode html / css language servers (vscode-langservers-extracted);
;;     both lsp-mode clients resolve their :system binary on PATH, so no
;;     client registration is needed -- only the hooks below.
;;   - prettier via apheleia
;;
;; Why html-ts-mode and not web-mode: this config drives everything through
;; tree-sitter (-ts-mode) and treesit-auto.  treesit-auto loads after this
;; file and force-prepends ("\\.html\\'" . html-ts-mode) onto
;; `auto-mode-alist', which would shadow a web-mode mapping anyway.  Svelte
;; components are handled by svelte-mode, so plain .html here is simple
;; markup that html-ts-mode handles cleanly.  html-ts-mode already maps to
;; the "html" lsp language id.

;;; Code:

;; =============================================================================
;; HTML  (built-in tree-sitter)
;; =============================================================================

(use-package html-ts-mode
  :ensure nil  ; built-in (Emacs 30+)
  :mode (("\\.html\\'" . html-ts-mode)
         ("\\.htm\\'"  . html-ts-mode)))

;; =============================================================================
;; CSS / SCSS  (built-in tree-sitter)
;; =============================================================================

(use-package css-ts-mode
  :ensure nil  ; built-in
  :mode (("\\.css\\'"  . css-ts-mode)
         ("\\.scss\\'" . css-ts-mode)))

;; =============================================================================
;; EMMET  (markup / css expansion)
;; =============================================================================

(use-package emmet-mode
  :ensure t
  :hook ((html-ts-mode . emmet-mode)
         (css-ts-mode  . emmet-mode))
  :bind (:map emmet-mode-keymap
              ("C-c TAB" . emmet-expand-line))
  :config
  (evil-define-key 'insert emmet-mode-keymap
    (kbd "M-w") #'emmet-expand-line))

;; =============================================================================
;; APHELEIA FORMATTER (prettier)
;; =============================================================================

(with-eval-after-load 'apheleia
  (dolist (m '(html-ts-mode css-ts-mode))
    (setf (alist-get m apheleia-mode-alist) 'prettier)))

;; =============================================================================
;; LSP  (html-ls / css-ls find the :system binaries on PATH)
;; =============================================================================
;; No client registration: html-ts-mode -> "html", css-ts-mode -> "css" are
;; already in `lsp-language-id-configuration', and the html/css clients
;; resolve their :system command (vscode-langservers-extracted) on PATH.

;; css-ts-mode reports language id "css" for every file it opens, including
;; .scss.  css-ls then validates SCSS as plain CSS and rejects // comments.
;; Prepending the file-pattern entry ensures .scss files get "scss" before
;; the mode-symbol entry is checked.
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '("\\.scss\\'" . "scss")))

(dolist (h '(html-ts-mode-hook
             css-ts-mode-hook))
  (add-hook h #'lsp-deferred))

(provide 'html-css-settings)
;;; html-css-settings.el ends here
