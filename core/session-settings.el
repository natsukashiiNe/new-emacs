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

;; (use-package perspective
;;   :ensure t
;;   :demand t
;;   :init
;;   (persp-mode)
;; 
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-c n"))
;;   (persp-suppress-no-prefix-key-warning t)
;;   (persp-state-default-file "~/.emacs.d/perspective-session")
;; 
;;   :bind ("C-c i" . persp-switch-last))

(defvar my-persp/settings (make-hash-table :test 'equal)
  "Hash-map for global persp-mode settings.
Currently stores:
  `last-visited' - name of the previously active perspective.")

(defun my-persp--save-current-before-switch (name &rest _)
  "Save current perspective name to `my-persp/settings' before switching.
NAME is the target perspective; ignored here."
  (let ((current (safe-persp-name (get-current-persp))))
    (unless (string= current name)
      (puthash 'last-visited current my-persp/settings))))

(defun my-persp/switch-to-last-visited ()
  "Switch to the last visited perspective."
  (interactive)
  (let ((last (gethash 'last-visited my-persp/settings)))
    (if last
        (persp-switch last)
      (message "No previous perspective recorded."))))

;; Per-daemon perspective isolation: each named daemon gets its own save
;; directory so daemons never overwrite each other's perspectives.
;; (daemonp) returns the daemon name string during init, before server-name is set.
(when-let* ((dn (and (stringp (daemonp)) (daemonp))))
  (setq persp-save-dir
        (expand-file-name (format "persp-confs/%s/" dn)
                          user-emacs-directory)))

(use-package persp-mode
  :ensure t
  :demand t
  :custom
  (persp-keymap-prefix (kbd "C-c n"))
  :config
  (make-directory persp-save-dir t)
  (add-hook 'persp-before-switch-functions #'my-persp--save-current-before-switch)
  (persp-mode)

  ;; Process buffer save/load handlers for daemon restart (reboot) recovery.
  ;; While the daemon is alive, all process buffers persist naturally in memory.
  ;; These handlers save enough metadata to recreate fresh buffers on next launch.

  (persp-def-buffer-save/load
   :mode 'compilation-mode
   :tag-symbol 'def-compilation-buffer
   :save-vars '(default-directory compilation-directory
				  compilation-environment compilation-arguments))

  (persp-def-buffer-save/load
   :mode 'shell-mode
   :tag-symbol 'def-shell-buffer
   :save-vars '(default-directory)
   :load-function
   (lambda (savelist &rest _)
     (let* ((buf-name (cadr savelist))
            (vars (caddr savelist))
            (dir (or (alist-get 'default-directory vars) "~/")))
       (or (get-buffer buf-name)
           (let ((default-directory dir))
             (shell buf-name))))))

  (with-eval-after-load 'vterm
    (persp-def-buffer-save/load
     :mode 'vterm-mode
     :tag-symbol 'def-vterm-buffer
     :save-vars '(default-directory)
     :load-function
     (lambda (savelist &rest _)
       (let* ((buf-name (cadr savelist))
              (vars (caddr savelist))
              (dir (or (alist-get 'default-directory vars) "~/")))
         (or (get-buffer buf-name)
             (let ((default-directory dir))
               (vterm buf-name))))))))

(use-package activities
  :ensure (:host github :repo "alphapapa/activities.el" )
  :demand t
  :init
  (activities-mode)
  (lambda () (activities-tabs-mode -1))
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :config
  (my-keymaps-set-activities-global-keymaps))


;; TODO
;; (use-package eyebrowse
;;   :ensure t
;;   :init
;;   (eyebrowse-mode t)
;;   :config
;;   (setq eyebrowse-new-workspace t)  ;; Always create new empty workspaces
;;   (setq eyebrowse-wrap-around t)    ;; Cycle when reaching the end
;;   (setq eyebrowse-mode-line-separator " | ")
;;   (setq eyebrowse-mode-line-style 'always)
;;   (setq eyebrowse-keymap-prefix (kbd "C-c w")))


(provide 'session-settings)
;;; session-settings ends here
