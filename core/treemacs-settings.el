;;; treemacs-settings.el --- Treemacs settings. -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings and supporting packages for treemacs.

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :bind
  ("C-x t t" . treemacs)
  :config
  ;; Remember the last project and expand state
  (setq treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-directory)
        treemacs-is-never-other-window t
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-width 35
        treemacs-show-cursor nil
        treemacs-sorting 'alphabetic-asc)

  ;; Start in current projectile root if not already open
  (defun my/treemacs-smart-toggle ()
    "Open Treemacs in the current Projectile project, or toggle it."
    (interactive)
    (if (treemacs-current-visibility)
        (treemacs)
      (let ((project-root (projectile-project-root)))
        (if project-root
            (treemacs-add-and-display-current-project-exclusively)
          (treemacs)))))
  
  (global-set-key (kbd "C-x t t") #'my/treemacs-smart-toggle))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (lsp treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-perspective
  :ensure t
  :after treemacs perspective
  :config (treemacs-set-scope-type 'Perspectives))


;; HOTKEYS
(with-eval-after-load 'treemacs
  (evil-define-key 'normal treemacs-mode-map
    (kbd "a") #'treemacs-create-file
    (kbd "A") #'treemacs-create-dir
    (kbd "r") #'treemacs-rename-file
    (kbd "x") #'treemacs-move-file
    ;; TODO move root (backspace)
    ;; (kbd "x") #'treemacs-move-file
    (kbd "TAB") #'treemacs-toggle-node))


(provide 'treeemacs-settings.el)
;;; treeemacs-settings ends here
