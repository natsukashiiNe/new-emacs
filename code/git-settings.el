;;; git-settings.el --- Setting for git. -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages for git repository control and its integration to the editor.

;;; Code:
(use-package magit
  :ensure t
  :after '(transient evil)
  :demand t
  :commands (magit-status magit-get-current-branch)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-save-repository-buffers nil)
  :config
  (defun my-magit-open-in-new-tab ()
    "Open Magit status in a new tab."
    (interactive)
    (tab-new)
    (magit-status))

  ;; KEYMAPS
  (evil-define-key 'normal magit-status-mode-map
    (kbd "o") #'magit-dispatch))


(use-package forge
  :ensure t
  :after magit)

(use-package vdiff-magit
  :ensure t
  :after (magit vdiff))

(provide 'git-settings)
;;; git-settings.el ends here
