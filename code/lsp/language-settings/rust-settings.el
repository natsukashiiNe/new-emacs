;;; rust-settings.el --- Settings for rust-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust language configuration: rust-analyzer LSP and tree-sitter mode.

;;; Code:

;; --- Tree-sitter mode ---
(use-package rust-ts-mode
  :ensure nil  ; built-in (Emacs 29+)
  :mode "\\.rs\\'")

;; --- Apheleia formatter ---
;; rustfmt is a built-in apheleia formatter; no formatter definition needed.
(with-eval-after-load 'apheleia
  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt))

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t))

;; Hook setup
(add-hook 'rust-ts-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)

(provide 'rust-settings)
;;; rust-settings.el ends here
