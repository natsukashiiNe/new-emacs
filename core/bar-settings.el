;;; bar-settings.el --- Settings for Emacs bars. -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings for tab-bar and mode-line.

;;; Code:

;; == mode-line =====================================================
(column-number-mode 1)

(use-package doom-modeline
  :ensure t
  :demand t
  :after evil
  :custom
  (doom-modeline-icon t)
  (doom-modeline-height 37)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon nil)
  ;; (doom-modeline-time-live-icon nil)

  (doom-modeline-column-zero-based t)
  (doom-modeline-position-line-format '("%l:"))
  (doom-modeline-position-column-format '("C%c"))
  (doom-modeline-indent-info t)
  (doom-modeline-persp-name nil)

  ;; git
  ;; TODO: remove
  (doom-modeline-vcs-icon nil)

  (doom-modeline-buffer-file-name-style 'relative-to-project)

  (doom-modeline-enable-word-count t)
  (doom-modeline-modal-icon t)

  :config
  (doom-modeline-mode t))

;; == tab-bar =====================================================
;; All expensive lookups are cached in plain strings and refreshed
;; by a single idle timer.  The tab-bar-format functions only ever
;; read those strings, so redisplay stays fast.

(defvar my/tab-bar--server-name-cache " [ ] ")
(defvar my/tab-bar--persp-name-cache nil)
(defvar my/tab-bar--git-branch-cache nil)
(defvar my/tab-bar--diagnostics-cache nil)

(defun my/tab-bar--refresh-cache ()
  "Recompute all tab-bar caches.  Called from an idle timer."
  ;; server name
  (setq my/tab-bar--server-name-cache
        (if (and (featurep 'server) (bound-and-true-p server-process))
            (concat " [" server-name "] ")
          " [ ] "))
  ;; perspective name: [server/persp-name|nil]
  (let ((srv (if (and (featurep 'server) (bound-and-true-p server-process))
                 server-name
               nil))
        (persp (when (bound-and-true-p persp-mode)
                 (safe-persp-name (get-current-persp)))))
    (setq my/tab-bar--persp-name-cache
          (concat " [" (or srv "") "/" (or persp "nil") "] ")))
  ;; git branch — use vc (built-in, fast) instead of magit
  (setq my/tab-bar--git-branch-cache
        (when-let* ((file (buffer-file-name))
                    (branch (vc-git--symbolic-ref file)))
          (let ((truncated (if (> (length branch) 30)
                               (concat (substring branch 0 27) "...")
                             branch)))
            (concat "  " truncated " "))))
  ;; flycheck diagnostics
  (setq my/tab-bar--diagnostics-cache
        (when (bound-and-true-p flycheck-current-errors)
          (let* ((counts (flycheck-count-errors flycheck-current-errors))
                 (errors   (or (cdr (assq 'error   counts)) 0))
                 (warnings (or (cdr (assq 'warning counts)) 0))
                 (infos    (or (cdr (assq 'info    counts)) 0)))
            (format " E:%d W:%d I:%d " errors warnings infos))))
  (force-mode-line-update t))

(run-with-idle-timer 1 t #'my/tab-bar--refresh-cache)

(defun my/tab-bar--refresh-persp-cache ()
  "Recompute only the perspective name cache and update the tab-bar."
  (let ((srv (if (and (featurep 'server) (bound-and-true-p server-process))
                 server-name
               nil))
        (persp (when (bound-and-true-p persp-mode)
                 (safe-persp-name (get-current-persp)))))
    (setq my/tab-bar--persp-name-cache
          (concat " [" (or srv "") "/" (or persp "nil") "] ")))
  (force-mode-line-update t))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-activated-functions
            (lambda (&rest _) (my/tab-bar--refresh-persp-cache)))
  (add-hook 'persp-created-functions
            (lambda (&rest _) (my/tab-bar--refresh-persp-cache)))
  (add-hook 'persp-renamed-functions
            (lambda (&rest _) (my/tab-bar--refresh-persp-cache)))
  (add-hook 'persp-before-kill-functions
            (lambda (&rest _)
              (run-with-timer 0.1 nil #'my/tab-bar--refresh-persp-cache))))

(defun my/tab-bar-server-name ()
  `((server menu-item ,my/tab-bar--server-name-cache ignore)))

(defun my/tab-bar-git-branch ()
  (when my/tab-bar--git-branch-cache
    `((git menu-item ,my/tab-bar--git-branch-cache ignore))))

(defun my/tab-bar-diagnostics ()
  (when my/tab-bar--diagnostics-cache
    `((diagnostics menu-item ,my/tab-bar--diagnostics-cache ignore))))

(defun my/tab-bar-persp-name ()
  `((persp menu-item ,my/tab-bar--persp-name-cache ignore)))

;; (defun my/tab-bar-tab-name-format (tab i)
;;   "Format TAB name with just a small padding around it."
;;   (let* ((current-p (eq (car tab) 'current-tab))
;;          (name (alist-get 'name tab))
;;          (str (concat " " name " ")))
;;     (if current-p
;;         (propertize str 'face 'tab-bar-tab)
;;       (propertize str 'face 'tab-bar-tab-inactive))))

;; (setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format)

(defun my/tab-bar-height-spacer ()
  "Invisible PBM image that forces tab-bar to match `doom-modeline-height'.
Same technique doom-modeline uses internally for its bar."
  (let* ((height (or (bound-and-true-p doom-modeline-height) 37))
         (img (when (display-graphic-p)
                (create-image
                 (format "P1\n1 %d\n%s\n" height (make-string height ?0))
                 'pbm t :scale 1 :ascent 'center))))
    (when img
      `((spacer menu-item ,(propertize " " 'display img) ignore)))))

(setq tab-bar-auto-width nil)
(setq tab-bar-close-button-show nil)
(setq tab-bar-separator " | ")
(setq tab-bar-tab-hints nil)

(setq tab-bar-format '(my/tab-bar-height-spacer
                       my/tab-bar-persp-name
                       tab-bar-format-tabs
                       tab-bar-format-align-right
                       my/tab-bar-git-branch
                       my/tab-bar-diagnostics))

(tab-bar-mode 1)

;; == frame title =====================================================
;; Shows "<server-name>: <persp-name> - GNU Emacs" in the X window title.
(setq frame-title-format
      '((:eval (if (and (featurep 'server) (bound-and-true-p server-process))
                   server-name
                 "[no server]"))
        ": "
        (:eval (if (bound-and-true-p persp-mode)
                   (safe-persp-name (get-current-persp))
                 "none"))
        " - GNU Emacs"))

;; == BARS MAPPINGS ===================================================


;; == STARTUP SCREEN ===================================================

(use-package enlight
  :ensure t
  :demand t
  ;; :hook (emacs-startup-hook . enlight-open)
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
