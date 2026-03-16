;;; setup-local-env.el --- Settings and packages for local env.  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-activate "~/src/venvs/emacs-venv")
  (setq python-shell-interpreter "python3")
  (pyvenv-mode 1))

;; ;; Optional: pyenv-mode if you also use pyenv for version management
;; ;; Comment out if you only use venv
;; (use-package pyenv-mode
;;   :straight t
;;   :if (executable-find "pyenv")
;;   :init
;;   (setq pyenv-mode-map nil)  ; Disable default keybindings
;;   :config
;;   (pyenv-mode 1))

;; Sudo edit utility
(use-package sudo-edit
  :ensure t)

(provide 'setup-local-env)
;;; setup-local-env.el
