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
