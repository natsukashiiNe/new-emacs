;;; git-settings.el --- Setting for git. -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages for git repository control and its integration to the editor.

;;; Code:
(use-package magit
  :ensure t
  :after (transient evil)
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

;; FIXME -> Currently does not work with magit
(use-package vdiff-magit
  :ensure t
  :after (magit vdiff))

(use-package difftastic
  :ensure (:url "https://github.com/pkryger/difftastic.el.git"
		:rev :newest)
  :after magit
  :config
  (difftastic-bindings-mode))

;; ==================================
;; diff-hl indicator config
;; ==================================


(use-package diff-hl
  :ensure t
  :demand t
  :init
  (defvar my-diff-hl-margin-symbols
    '((insert . "")
      (delete . "")
      (change . "")
      (unknown . "")
      (ignored . ""))
    "Margin symbols for diff-hl matching neovim gitsigns.")
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  :config
  ;; Use margin mode everywhere (nerd font chars instead of fringe bitmaps)
  (setq diff-hl-margin-symbols-alist my-diff-hl-margin-symbols)
  (diff-hl-margin-mode)
  ;; Magit integration
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; TODO: Navigate hunks: C-x v [ and C-x v ]
  ;; (define-key diff-hl-mode-map (kbd "M-[") 'diff-hl-previous-hunk)
  ;; (define-key diff-hl-mode-map (kbd "M-]") 'diff-hl-next-hunk)

  ;; Show diff popup for current hunk
  (define-key diff-hl-mode-map (kbd "C-x v p") 'diff-hl-show-hunk)

  ;; To make flycheck appear on top of diff-hl:
  (setq-default diff-hl-side 'left))


(provide 'git-settings)
;;; git-settings.el ends here
