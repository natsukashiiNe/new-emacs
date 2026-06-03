;;; dotnet-settings.el --- Settings for csharp-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; C# / .NET language configuration: omnisharp LSP, sharper, csproj-mode.

;;; Code:

;; --- Indent bars scope ---
(with-eval-after-load 'indent-bars
  (add-to-list 'indent-bars-treesit-scope
               '(c-sharp class_declaration method_declaration if_statement for_statement
                         while_statement switch_statement try_statement namespace_declaration)))

;; --- Tree-sitter mode ---
(use-package csharp-ts-mode
  :ensure nil
  :defer t
  :mode "\\.cs\\'")

;; --- Apheleia formatter ---
(with-eval-after-load 'apheleia
  (setf (alist-get 'csharpier apheleia-formatters)
        '("csharpier" "format" "--write-stdout" "--stdin-path" filepath))
  (setf (alist-get 'csharp-mode apheleia-mode-alist) 'csharpier)
  (setf (alist-get 'csharp-ts-mode apheleia-mode-alist) 'csharpier))

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

(provide 'dotnet-settings)
;;; dotnet-settings.el ends here
