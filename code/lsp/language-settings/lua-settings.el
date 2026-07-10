;;; lua-settings.el --- Settings for lua-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Lua language configuration: lua-language-server and lua-mode.

;;; Code:

;; --- Mode ---
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

;; --- Configuration Variables ---
(defcustom my/lua-workspace-library-paths
  '("/usr/share/nvim/runtime/lua"
    "/usr/share/nvim/runtime/lua/vim"
    "/usr/share/awesome/lib")
  "Additional workspace library paths for Lua language server.
Useful for Neovim plugin development or Awesome WM configuration."
  :type '(repeat string)
  :group 'my-lsp)

(defun my/lua-workspace-library-hashtable ()
  "Convert `my/lua-workspace-library-paths' to a hashtable for 'lsp-mode'."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (path my/lua-workspace-library-paths)
      (puthash path t ht))
    ht))

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  (setq lsp-lua-runtime-version "LuaJIT")
  (setq lsp-lua-diagnostics-globals ["vim" "awesome" "client" "root" "screen"])
  (setq lsp-lua-workspace-library (my/lua-workspace-library-hashtable))
  (setq lsp-lua-workspace-check-third-party nil)
  (setq lsp-lua-telemetry-enable nil))

;; Hook setup
(add-hook 'lua-mode-hook #'lsp-deferred)

(provide 'lua-settings)
;;; lua-settings.el ends here
