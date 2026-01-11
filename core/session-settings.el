;;; session-settings.el --- Better built-in modes for Emacs. -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhancments for built-in modes.

;;; Code:
(use-package projectile
  :ensure t
  :demand t
  :init
  (setq projectile-project-search-path '("/root"))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default
        projectile-indexing-method 'native
        projectile-enable-caching t              ; Enable caching
        projectile-git-command "git ls-files -zco --exclude-standard"
        projectile-git-submodule-command nil)
  ;; TODO: move to a proper keymap file.
  :bind (:map projectile-mode-map
	      ("C-c b" . consult-project-buffer))
  :bind-keymap
  ("C-c o" . projectile-command-map))

(use-package perspective
  :ensure t
  :demand t
  :init
  (persp-mode)

  :custom
  (persp-mode-prefix-key (kbd "C-c n"))
  (persp-suppress-no-prefix-key-warning t)
  (persp-state-default-file "~/.emacs.d/perspective-session")

  :bind ("C-c i" . persp-switch-last))

(provide 'session-settings)
;;; session-settings ends here
