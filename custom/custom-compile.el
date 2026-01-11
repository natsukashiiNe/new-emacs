;;; custom-compile.el --- Custom compilation commands. -*- lexical-binding: t; -*-

;;; Commentary:
;; Define quick-access compilation commnad per project and map them to keys.

;;; Code:

(defun my/compile-with-name (name-suffix &optional command)
  "Run compilation with project-specific buffer named *NAME-SUFFIX:project*."
  (let* ((project (projectile-project-name))
         (default-directory (projectile-project-root))
         (compilation-buffer-name-function
          (lambda (_mode) (format "*%s:%s*" name-suffix project)))
         ;; Prevent compile from overwriting buffer-local compile-command
         (compile-command (or command compile-command)))
    (compile compile-command)))

(defun my/project-compile ()
  (interactive)
  (my/compile-with-name "compile" projectile-project-compilation-cmd))

(defun my/project-run ()
  (interactive)
  (my/compile-with-name "run" projectile-project-run-cmd))

(defun my/project-test ()
  (interactive)
  (my/compile-with-name "test" projectile-project-test-cmd))

(defun my/project-custom (name command)
  "Run arbitrary named compilation."
  (interactive "sBuffer name: \nsCommand: ")
  (my/compile-with-name name command))

;; FIX: have this be written to the file / .dir-locals.el
(defun my/reload-dir-locals-on-save ()
  "Reload dir-locals for project when .dir-locals.el is saved."
  (when (string-equal (file-name-nondirectory buffer-file-name) ".dir-locals.el")
    (setq dir-locals-class-alist nil
          dir-locals-directory-cache nil)
    (dolist (buf (projectile-project-buffers))
      (with-current-buffer buf
        (hack-dir-local-variables-non-file-buffer)))
    (message "Dir-locals reloaded for project")))

(add-hook 'after-save-hook #'my/reload-dir-locals-on-save)


;; TESTING

(defun my/project-custom-comint (name command)
  "Run arbitrary named compilation with comint mode (interactive input)."
  (interactive "sBuffer name: \nsCommand: ")
  (let* ((project (projectile-project-name))
         (default-directory (projectile-project-root))
         (compilation-buffer-name-function
          (lambda (_mode) (format "*%s:%s*" name project))))
    ;; 't' as the mode argument to enable comint-mode
    (compilation-start command t)))

(dolist (var '(compile-command
               projectile-project-run-cmd
               my/project-custom-comint
               my/project-custom))
  (put var 'risky-local-variable nil)
  (put var 'safe-local-variable #'stringp))

(provide 'custom-compile)
;;; custom-compile.el ends here
