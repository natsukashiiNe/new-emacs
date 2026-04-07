;;; cpp-lsp-settings.el --- Settings for c++-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; C/C++ language configuration: clangd LSP with Unreal Engine support,
;; tree-sitter modes for C and C++.

;;; Code:

;; --- Tree-sitter modes ---
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

(defcustom my/clangd-ue-args
  '("--background-index"
    "--completion-style=detailed"
    "--header-insertion=never"
    "--pch-storage=disk"
    "--log=error")
  "Clangd arguments for Unreal Engine projects.
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

(provide 'cpp-lsp-settings)
;;; cpp-lsp-settings.el ends here
