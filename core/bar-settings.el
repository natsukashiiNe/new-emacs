;;; bar-settings.el --- Settings for Emacs bars. -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings for tab-bar and mode-line.

;;; Code:

;; == mode-line =====================================================
(use-package doom-modeline
  :ensure t
  :demand t
  :after evil
  :custom
  (doom-modeline-height 30)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-modal-icon t)
  :config
  (doom-modeline-mode t))


;; == tab-bar =====================================================
;; (require magit)

(setq tab-bar-format '(;;my/tab-bar-server-name
		       tab-bar-format-tabs
                       tab-bar-separator
                       tab-bar-format-align-right
                       my/tab-bar-git-branch
                       ;; my/tab-bar-git-diff
                       my/tab-bar-diagnostics))

(defun my/tab-bar-git-branch ()
  "Show git branch in tab-bar."
  (when (and (featurep 'magit) (magit-get-current-branch))
    `((git menu-item
           ,(concat "  " (magit-get-current-branch) " ")
           ignore
           :help "Git branch"))))

(defun my/tab-bar-diagnostics ()
  "Show LSP/flycheck diagnostics in tab-bar."
  (when (bound-and-true-p flycheck-mode)
    (let* ((counts (flycheck-count-errors flycheck-current-errors))
           (errors (or (cdr (assq 'error counts)) 0))
           (warnings (or (cdr (assq 'warning counts)) 0))
           (infos (or (cdr (assq 'info counts)) 0)))
      `((diagnostics menu-item
                     ,(format " E:%d W:%d I:%d " errors warnings infos)
                     ignore
                     :help "Diagnostics")))))

(defun my/tab-bar-server-name ()
  "Show server name or [ ] if no server."
  (require 'server)
  `((server menu-item
            ,(if (server-running-p)
                 (concat " [" server-name "] ")
               " [ ] ")
            ignore
            :help ,(if (server-running-p)
                       (concat "Server: " server-name)
                     "No server running"))))

(tab-bar-mode 1)

;; == BARS MAPPINGS ===================================================


;; == STARTUP SCREEN ===================================================

(use-package enlight
  :ensure t
  :demand t
  :hook (emacs-startup-hook . enlight-open)
  :custom
  (enlight-content
   (concat
    (propertize "MENU" 'face 'highlight)
    "\n"
    (enlight-menu
     '(("Org Mode"
	("Org-Agenda (current day)" (org-agenda nil "a") "a"))
       ("Downloads"
	("Transmission" transmission "t")
	("Downloads folder" (dired "~/Downloads") "a"))
       ("Other"
	("Projects" project-switch-project "p")))))))

(provide 'bar-settings)
;;; bar-settings.el ends here
