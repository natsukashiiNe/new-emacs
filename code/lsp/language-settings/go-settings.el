;;; go-settings.el --- Settings for go-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Go language configuration: gopls LSP and tree-sitter mode.

;;; Code:

;; --- Indent bars scope ---
(with-eval-after-load 'indent-bars
  (add-to-list 'indent-bars-treesit-scope
               '(go function_declaration method_declaration func_literal if_statement
                    for_statement switch_statement select_statement)))

;; --- Tree-sitter mode ---
(use-package go-ts-mode
  :ensure nil  ; built-in (Emacs 29+)
  :mode "\\.go\\'")

;; --- Apheleia formatter ---
;; gofmt is a built-in apheleia formatter; no formatter definition needed.
(with-eval-after-load 'apheleia
  (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofmt))

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  (setq lsp-go-use-placeholders t)
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)))
  (setq lsp-go-codelenses '((generate . t)
                            (regenerate_cgo . t)
                            (test . t)
                            (tidy . t)
                            (upgrade_dependency . t)
                            (vendor . t))))

;; Hook setup
(add-hook 'go-ts-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)

(provide 'go-settings)
;;; go-settings.el ends here
