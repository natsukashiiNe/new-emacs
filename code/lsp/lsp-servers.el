;;; lsp-servers.el --- Language-specific LSP server configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; DEPRECATED: Language server configurations have been moved to
;; individual files under code/lsp/servers/:
;;
;;   cpp-lsp-settings.el      — C/C++ (clangd)
;;   python-lsp-settings.el   — Python (pyright)
;;   rust-lsp-settings.el     — Rust (rust-analyzer)
;;   go-lsp-settings.el       — Go (gopls)
;;   java-lsp-settings.el     — Java (Eclipse JDT)
;;   lua-lsp-settings.el      — Lua (lua-language-server)
;;   dotnet-lsp-settings.el   — C# / .NET (omnisharp, sharper, csproj)
;;   minor-langs-settings.el  — Bash, CMake, Yuck, KDL, JSON, YAML, TOML, Dockerfile
;;
;; All servers are now loaded via `require' from lsp-setup.el.

;;; Code:

(provide 'lsp-servers)
;;; lsp-servers.el ends here
