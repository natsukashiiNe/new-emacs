;;; lsp-servers.el --- Language-specific LSP server configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Language-specific LSP server configurations.
;; Requires lsp-setup.el to be loaded first.

;;; Code:

;; =============================================================================
;; PYTHON - pyright + ruff (add-on)
;; =============================================================================

;; lsp-pyright auto-detects .venv/venv in project root.
;; Pyright installed globally via pipx; only runtime deps needed in project .venv.
(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-auto-search-paths t)
  (lsp-pyright-diagnostic-mode "workspace")
  (lsp-pyright-type-checking-mode "basic")
  :hook ((python-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)))


;; =============================================================================
;; C/C++ - clangd
;; =============================================================================

;; --- Configuration Variables ---
(defcustom my/clangd-compile-commands-patterns '("." "build" "build/Release" "build/Debug")
  "Directory patterns to search for compile_commands.json.
Relative to project root, searched in order."
  :type '(repeat string)
  :group 'my-lsp)

(defcustom my/clangd-fallback-build-dir "build/Release"
  "Default build directory when compile_commands.json not found."
  :type 'string
  :group 'my-lsp)

(defcustom my/clangd-executable "/usr/bin/clangd"
  "Path to clangd executable."
  :type 'string
  :group 'my-lsp)

(defcustom my/clangd-default-args
  '("--background-index"
    "--clang-tidy"
    "--completion-style=detailed"
    "--header-insertion=iwyu"
    "--pch-storage=memory"
    "--log=error")
  "Default clangd arguments for regular C++ projects."
  :type '(repeat string)
  :group 'my-lsp)

;; (defcustom my/clangd-ue-args
;;   '("--background-index"
;;     "--completion-style=detailed"
;;     "--header-insertion=never"
;;     "--pch-storage=memory"
;;     "--log=error")
;;   "clangd arguments for Unreal Engine projects."
;;   :type '(repeat string)
;;   :group 'my-lsp)

(defcustom my/clangd-ue-args
  '("--background-index"
    "--completion-style=detailed"
    "--header-insertion=never"
    "--pch-storage=disk"
    "--log=error")
  "clangd arguments for Unreal Engine projects.
Disables clang-tidy (noisy on UE macros), uses disk PCH storage
for persistence across restarts, and disables header insertion
since UE has its own include conventions."
  :type '(repeat string)
  :group 'my-lsp)

(defcustom my/ue-project-roots '("/storage/gacha/projects/simulation")
  "List of Unreal Engine project roots that need special clangd args."
  :type '(repeat string)
  :group 'my-lsp)

;; --- Resolution Functions ---
(defun my/clangd--ue-project-p (project-root)
  "Return t if PROJECT-ROOT is an Unreal Engine project."
  (seq-some (lambda (ue-root)
              (string-prefix-p ue-root project-root))
            my/ue-project-roots))

(defun my/clangd--resolve-compile-commands (project-root)
  "Resolve compile_commands.json directory for PROJECT-ROOT."
  (if-let* ((found-dir (my/clangd--find-compile-commands-in-patterns project-root)))
      (expand-file-name found-dir project-root)
    (expand-file-name my/clangd-fallback-build-dir project-root)))

(defun my/clangd--find-compile-commands-in-patterns (project-root)
  "Search for compile_commands.json in PROJECT-ROOT using configured patterns."
  (seq-find
   (lambda (dir-name)
     (let ((full-path (expand-file-name dir-name project-root)))
       (file-readable-p (expand-file-name "compile_commands.json" full-path))))
   my/clangd-compile-commands-patterns))

;; --- LSP Client Registration ---
(with-eval-after-load 'lsp-clangd

  (defun my/clangd--get-args ()
    "Return clangd args based on current project type."
    (let* ((root (lsp-workspace-root))
           (base-args (if (and root (my/clangd--ue-project-p root))
                          my/clangd-ue-args
                        my/clangd-default-args))
           (compile-dir (when root
                          (my/clangd--resolve-compile-commands root))))
      (append base-args
              (when compile-dir
                (list (concat "--compile-commands-dir=" compile-dir))))))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (cons (or lsp-clients-clangd-executable
				 lsp-clients--clangd-default-executable
				 "clangd")
                             (my/clangd--get-args))))
    :activation-fn (lsp-activate-on "c" "cpp" "objective-c")
    :priority -1
    :server-id 'clangd
    :library-folders-fn (lambda (_workspace) lsp-clients-clangd-library-directories)
    :async-request-handlers
    (ht ("textDocument/ast" #'lsp-clangd--ast-handler)))))

;; Hook setup
(add-hook 'c-ts-mode-hook #'lsp-deferred)
(add-hook 'c++-ts-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

;; =============================================================================
;; LUA - lua-language-server
;; =============================================================================

;; --- Configuration Variables ---
(defcustom my/lua-workspace-library-paths
  '("/usr/share/nvim/runtime/lua"
    "/usr/share/nvim/runtime/lua/vim"
    "/usr/share/awesome/lib")
  "Additional workspace library paths for Lua language server.
Useful for Neovim plugin development or Awesome WM configuration."
  :type '(repeat string)
  :group 'my-lsp)

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  ;; Configure lua-language-server
  (setq lsp-lua-runtime-version "LuaJIT")
  (setq lsp-lua-diagnostics-globals '("vim" "awesome" "client" "root" "screen"))
  (setq lsp-lua-workspace-library my/lua-workspace-library-paths)
  (setq lsp-lua-workspace-check-third-party nil)
  (setq lsp-lua-telemetry-enable nil))

;; Hook setup
(add-hook 'lua-mode-hook #'lsp-deferred)

;; =============================================================================
;; GO - gopls
;; =============================================================================

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  ;; Configure gopls
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

;; =============================================================================
;; RUST - rust-analyzer
;; =============================================================================

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  ;; Configure rust-analyzer
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t))

;; Hook setup
(add-hook 'rust-ts-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)

;; =============================================================================
;; BASH - bash-language-server
;; =============================================================================

;; Hook setup (no special configuration needed)
(add-hook 'bash-ts-mode-hook #'lsp-deferred)
(add-hook 'sh-mode-hook #'lsp-deferred)

;; =============================================================================
;; CMAKE - cmake-language-server
;; =============================================================================

;; Hook setup (no special configuration needed)
(add-hook 'cmake-ts-mode-hook #'lsp-deferred)
(add-hook 'cmake-mode-hook #'lsp-deferred)

;; =============================================================================
;; JAVA - Eclipse JDT Language Server
;; =============================================================================

;; --- Configuration Variables ---
(defcustom my/java-runtime-path "/usr/lib/jvm/java-23-openjdk"
  "Path to Java runtime for JDT language server."
  :type 'string
  :group 'my-lsp)

(defcustom my/java-format-settings-path "~/.java_formatting.xml"
  "Path to Java code formatting settings XML file."
  :type 'string
  :group 'my-lsp)

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  ;; Configure Java settings
  (setq lsp-java-configuration-runtimes
        `[(:name "JavaSE-23"
		 :path ,my/java-runtime-path
		 :default t)])
  
  (setq lsp-java-format-settings-url
        (expand-file-name my/java-format-settings-path))
  
  (setq lsp-java-format-enabled t))

;; Hook setup
(add-hook 'java-ts-mode-hook #'lsp-deferred)
(add-hook 'java-mode-hook #'lsp-deferred)

;; =============================================================================
;; LANGUAGE MODE CONFIGURATIONS
;; =============================================================================

;; Setup tree-sitter and traditional mode associations
;; These ensure the correct major modes are activated

(use-package c-ts-mode
  :ensure nil  ; built-in
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode))
  :config
  (setq c-ts-mode-indent-offset 4))

(use-package c++-ts-mode
  :ensure nil  ; built-in
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hh\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode)
         ("\\.cppm\\'" . c++-ts-mode))  ; C++20 modules
  :config
  (setq c-ts-mode-indent-offset 4))

(use-package python-ts-mode
  :ensure nil  ; built-in
  :mode "\\.py\\'"
  :config
  (setq python-indent-offset 4))

(use-package go-ts-mode
  :ensure nil  ; built-in (Emacs 29+)
  :mode "\\.go\\'")

(use-package rust-ts-mode
  :ensure nil  ; built-in (Emacs 29+)
  :mode "\\.rs\\'")

(use-package bash-ts-mode
  :ensure nil  ; built-in
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("bashrc\\'" . bash-ts-mode)
         ("bash_profile\\'" . bash-ts-mode)))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package json-ts-mode
  :ensure nil  ; built-in
  :mode "\\.json\\'")

(use-package yaml-ts-mode
  :ensure nil  ; built-in
  :mode "\\.ya?ml\\'")

(use-package toml-ts-mode
  :ensure nil  ; built-in
  :mode "\\.toml\\'")

(use-package dockerfile-ts-mode
  :ensure nil  ; built-in (Emacs 29.1+)
  :mode "Dockerfile\\'")

(use-package cmake-ts-mode
  :ensure nil  ; built-in
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; .kdl mode
(use-package kdl-mode
  :ensure t
  :mode "\\.kdl\\'"
  :hook (kdl-mode . (lambda ()
                      ;; Enable tree-sitter if available
                      (when (treesit-available-p)
                        (treesit-parser-create 'kdl))))
  :config
  ;; Configure lsp-mode for KDL
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(kdl-mode . "kdl"))
    
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "kdl-lsp")
      :activation-fn (lsp-activate-on "kdl")
      :server-id 'kdl-lsp)))
  
  ;; Auto-start lsp-mode when opening KDL files (optional)
  ;; (add-hook 'kdl-mode-hook 'lsp-deferred)
  )

(provide 'lsp-servers)
;;; lsp-servers.el ends here
