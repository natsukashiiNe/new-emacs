;;; rust-lsp-settings.el --- Settings for rust-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust language configuration: rust-analyzer LSP and tree-sitter mode.

;;; Code:

;; --- Indent bars scope ---
(with-eval-after-load 'indent-bars
  (add-to-list 'indent-bars-treesit-scope
               '(rust function_item impl_item if_expression for_expression while_expression
                      match_expression loop_expression mod_item)))

;; --- Tree-sitter mode ---
(use-package rust-ts-mode
  :ensure nil  ; built-in (Emacs 29+)
  :mode "\\.rs\\'")

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t))

;; Hook setup
(add-hook 'rust-ts-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)

(provide 'rust-lsp-settings)
;;; rust-lsp-settings.el ends here
