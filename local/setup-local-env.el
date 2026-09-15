;;; setup-local-env.el --- Settings and packages for local env.  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(add-to-list 'exec-path "/home/nane/.npm-global/bin/")

(defvar my-local-env/cache-file (expand-file-name
				 "exec-path-cache.el"
				 my-config-dir))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config

;; I suppose pyright-lsp does it fine anyway.
;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (pyvenv-activate "~/src/venvs/emacs-venv")
;;   (setq python-shell-interpreter "python3")
;;   (pyvenv-mode 1))

;; ;; Optional: pyenv-mode if you also use pyenv for version management
;; ;; Comment out if you only use venv
;; (use-package pyenv-mode
;;   :straight t
;;   :if (executable-find "pyenv")
;;   :init
;;   (setq pyenv-mode-map nil)  ; Disable default keybindings
;;   :config
;;   (pyenv-mode 1))
  (if (file-exists-p my-local-env/cache-file)
      (load my-local-env/cache-file)
    (exec-path-from-shell-initialize)
    (with-temp-file my-local-env/cache-file
      (insert (format "(setq exec-path '%S)\n" exec-path))
      (insert (format "(setenv \"PATH\" %S)\n" (getenv "PATH"))))))

;; Sudo edit utility
(use-package sudo-edit
  :ensure t
  :defer t)

(provide 'setup-local-env)
;;; setup-local-env.el
