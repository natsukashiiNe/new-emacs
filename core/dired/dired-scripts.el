;;; scripts.el --- Custom dirvish scripts. -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom scripts for dirvish-narrow and other utilities.

;;; Code:

(require 'dirvish)

(defun my/dirvish-fd-narrow ()
  "Run `dirvish-fd' in project root, then `dirvish-narrow'.
If already in a project, use current project root.
Otherwise, prompt to select a project first."
  (interactive)
  (let* ((current-project (projectile-project-root))
         (project-action (lambda ()
                           (let ((project-root (projectile-project-root)))
                             (dirvish-fd project-root "")
                             (dirvish-narrow)))))
    (if current-project
        (funcall project-action)
      (let ((projectile-switch-project-action project-action))
        (condition-case nil
            (call-interactively 'projectile-switch-project)
          (quit nil))))))


(provide 'dired-scripts)
;;; scripts.el ends here
