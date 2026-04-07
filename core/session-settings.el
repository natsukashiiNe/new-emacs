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
  ("C-c C-c" . projectile-command-map))

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
  "Hash-map for global 'persp-mode' settings.
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

;; --- Window config normalization for cross-display persistence ---

(defun my-persp--normalize-wconf (wconf)
  "Normalize WCONF for display-independent save/restore.
Strips pixel dimensions and zeroes character dimensions so that
`window-state-put' uses normalized proportions (normal-height,
normal-width) instead of absolute sizes.  Sets head constraints
to minimal values so restoration never fails size checks."
  (when wconf
    (cons (my-persp--normalize-wconf-head (car wconf))
          (my-persp--normalize-wconf-node (cdr wconf)))))

(defun my-persp--normalize-wconf-head (_head)
  "Return minimal constraints alist for window state head."
  '((min-height . 1) (min-width . 1)
    (min-height-ignore . 1) (min-width-ignore . 1)
    (min-height-safe . 1) (min-width-safe . 1)
    (min-pixel-height . 1) (min-pixel-width . 1)
    (min-pixel-height-ignore . 1) (min-pixel-width-ignore . 1)
    (min-pixel-height-safe . 1) (min-pixel-width-safe . 1)))

(defun my-persp--normalize-wconf-node (node)
  "Recursively normalize a window state NODE.
NODE is a list starting with type symbol (leaf, vc, hc), followed
by alist entries and (for vc/hc) child nodes.
Removes pixel-width/pixel-height so `window-state-put' disables
pixelwise mode.  Zeros total-width/total-height so the totals
check fails and proportional (normal-*) sizing is used instead."
  (when (and node (listp node))
    (let ((type (car node)))
      (if (memq type '(leaf vc hc))
          (let (props children)
            (dolist (item (cdr node))
              (cond
               ;; Child node: recurse
               ((and (listp item) (memq (car-safe item) '(leaf vc hc)))
                (push (my-persp--normalize-wconf-node item) children))
               ;; Strip pixel dimensions entirely
               ((and (consp item) (memq (car item) '(pixel-width pixel-height)))
                nil)
               ;; Zero out character dimensions to force proportional mode
               ((and (consp item) (memq (car item) '(total-width total-height)))
                (push (cons (car item) 0) props))
               ;; Keep everything else
               (t (push item props))))
            (append (list type) (nreverse props) (nreverse children)))
        node))))

(use-package persp-mode
  :ensure t
  :demand t
  :custom
  (persp-keymap-prefix (kbd "C-c n"))
  :config
  ;; Per-daemon perspective isolation: each named daemon gets its own save
  ;; directory so daemons never overwrite each other's perspectives.
  ;; Must be inside :config to override no-littering's persp-save-dir.
  ;; Non-daemon and unnamed-daemon instances disable auto-save/restore entirely.
  (if-let* ((dn (and (stringp (daemonp)) (daemonp))))
      (setq persp-save-dir
            (expand-file-name (format "persp-confs/%s/" dn)
                              user-emacs-directory)
            persp-auto-resume-time 1.0
            persp-auto-save-opt 1)
    (setq persp-auto-resume-time -1
          persp-auto-save-opt 0))
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
               (vterm buf-name)))))))

  ;; --- Display-independent window config normalization ---
  ;; `window-state-get' includes absolute pixel/char dimensions that prevent
  ;; cross-display restoration (GUI->TTY, different frame sizes).
  ;; `window-state-put' has a fallback using `normal-height'/`normal-width'
  ;; (0.0-1.0 proportions) when absolute dimensions don't match the target.
  ;; By stripping absolute values at save time, restoration works everywhere.
  (setq persp-window-state-get-function
        (lambda (_frame rwin &optional writable)
          (when rwin
            (my-persp--normalize-wconf (window-state-get rwin writable)))))

  ;; Fix upstream bug: `persp-rename' references `old-name' without binding it.
  (define-advice persp-rename
      (:around (orig-fn new-name &optional persp phash) fix-old-name)
    "Bind `old-name' so the interactive prompt works."
    (let ((persp (or persp (get-current-persp)))
          (phash (or phash *persp-hash*)))
      (let ((old-name (safe-persp-name persp)))
        (funcall orig-fn new-name persp phash)))))

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
