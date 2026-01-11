;;; completion-scripts.el --- Scripts for consult. -*- lexical-binding: t; -*-

;;; Commentary:
;; This file has some usefull pickers for consult+vertico.

;;; Code:
;; Custom consult-fd with preview

(defun my/consult-fd-project (&optional dir initial)
  "Async fd search in project with preview (Telescope-like)."
  (interactive "P")
  (let* ((project (project-current))
         (default-directory (or dir
				(when project (project-root project))
				default-directory)))
    (find-file
     (consult--read
      (consult--async-command
       (lambda (input)
         (list "fd" "--type" "f" "--hidden" "--follow"
               "--exclude" ".git" "--color=never"
               "--full-path" (or input "")))
       (consult--async-split-initial initial))
      :prompt (format "Find file in %s: " default-directory)
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :initial initial
      :history 'file-name-history))))



(use-package consult
  :ensure t
  :demand t
  :config
  ;; Preview settings
  (setq consult-preview-key 'any)
  (setq consult-async-min-input 0) ; Show results immediately
  (setq consult-async-refresh-delay 0.2)
  (setq consult-async-input-debounce 0.15)
  (setq consult-async-input-throttle 0.3)

  ;; Configure fd args to your liking
  (setq consult-fd-args
        '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--full-path --color=never"
          "--hidden --exclude .git"))

  ;; Project-aware consult-fd
  (defun my/consult-fd-project ()
    "Search for files with fd in project root with preview."
    (interactive)
    (let ((default-directory
           (if-let ((project (project-current)))
               (project-root project)
             default-directory)))
      (consult-fd default-directory)))

  :bind (;; Minibuffer navigation
         :map minibuffer-local-map
         ("C-j" . next-line-or-history-element)
         ("C-k" . previous-line-or-history-element)
         ;; Project file finder
         ("C-c p f" . my/consult-fd-project)))


;; FIX: does not do caching per-project
(defvar my-consult-source-projectile-file
  `(:name "Project File"
          :narrow ?f
          :category file
          :face consult-file
          :history file-name-history
          :state ,(consult--file-preview)
          :items ,(lambda ()
                    (when-let ((root (projectile-acquire-root)))
                      (let ((files (projectile-dir-files root)))
                        (mapcar (lambda (file)
                                  (expand-file-name file root))
                                files)))))
  "Projectile file source for consult with preview.")

(defun my-consult-projectile-find-file ()
  "Find project file with preview, respecting .projectile ignore rules."
  (interactive)
  (let ((projectile-enable-caching nil))
    (find-file
     (consult--read
      (funcall (plist-get my-consult-source-projectile-file :items))
      :prompt "Project file: "
      :state (plist-get my-consult-source-projectile-file :state)
      :category 'file
      :sort nil
      :require-match t
      :history 'file-name-history
      :preview-key '(:debounce 0.2 any)))))



(provide 'completion-scripts)
;;; completion-scripts.el ends here
