;;; python-lsp-settings.el --- Settings for python-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Python language configuration: pyright LSP, tree-sitter mode,
;; and rebinding of python-ts-mode-map actions under C-c h prefix.

;;; Code:

;; --- Indent bars scope ---
(with-eval-after-load 'indent-bars
  (add-to-list 'indent-bars-treesit-scope
               '(python function_definition class_definition for_statement
                        if_statement with_statement while_statement try_statement)))

;; --- Tree-sitter mode ---
(use-package python-ts-mode
  :ensure nil  ; built-in
  :mode "\\.py\\'"
  :config
  (setq python-indent-offset 4))

;; --- LSP Server ---
(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-auto-search-paths t)
  (lsp-pyright-diagnostic-mode "openFiles")
  (lsp-pyright-type-checking-mode "basic")
  :hook ((python-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)))

;; --- Rebind python-ts-mode-map under C-c h prefix ---
;; Python's default bindings pollute C-c C-* which we use for global maps.
;; Wipe the entire keymap, then build only C-c h with python actions.
(with-eval-after-load 'python
  ;; Nuke all default bindings
  (setcdr python-ts-mode-map nil)

  ;; Rebuild under C-c h
  (keymap-set python-ts-mode-map "C-c h"
              (define-keymap
                ;; shell / eval
                "b"   #'python-shell-send-block
                "c"   #'python-shell-send-buffer
                "d"   #'python-describe-at-point
                "e"   #'python-shell-send-statement
                "f"   #'python-eldoc-at-point
                "h"   #'imenu
                "l"   #'python-shell-send-file
                "p"   #'run-python
                "r"   #'python-shell-send-region
                "S"   #'python-shell-send-string
                "v"   #'python-check
                "x"   #'python-shell-send-defun
                "z"   #'python-shell-switch-to-shell
                ;; indent
                "<"   #'python-indent-shift-left
                ">"   #'python-indent-shift-right
                ;; imports
                "i a" #'python-add-import
                "i f" #'python-fix-imports
                "i r" #'python-remove-import
                "i s" #'python-sort-imports
                ;; skeletons
                "s c" #'python-skeleton-class
                "s d" #'python-skeleton-def
                "s f" #'python-skeleton-for
                "s i" #'python-skeleton-if
                "s m" #'python-skeleton-import
                "s t" #'python-skeleton-try
                "s w" #'python-skeleton-while)))

(with-eval-after-load 'python
  (which-key-add-keymap-based-replacements python-ts-mode-map
    "C-c h s" "skeletons"
    "C-c h i" "imports"))

(provide 'python-lsp-settings)
;;; python-lsp-settings.el ends here
