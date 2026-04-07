;;; dotnet-lsp-settings.el --- Settings for csharp-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; C# / .NET language configuration: omnisharp LSP, sharper, csproj-mode.

;;; Code:

;; --- Tree-sitter mode ---
(use-package csharp-ts-mode
  :ensure nil
  :defer t
  :mode "\\.cs\\'")

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-csharp
  (setq lsp-csharp-omnisharp-enable nil))

(add-hook 'csharp-ts-mode-hook #'lsp-deferred)
(add-hook 'csharp-mode-hook #'lsp-deferred)

;; --- .NET tooling ---
(use-package sharper
  :ensure t
  :defer t
  :bind ("C-c d" . sharper-main-transient))

(use-package csproj-mode
  :ensure t
  :defer t)

(provide 'dotnet-lsp-settings)
;;; dotnet-lsp-settings.el ends here
