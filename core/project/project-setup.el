;;; project-setup.el --- Settings for project management. -*- lexical-binding: t; -*-

;;; Commentary:
;; project.el customisation: teaches it to recognise .project-locals.el
;; as a project root marker (higher priority than .git), and wires lsp-mode
;; root detection to use project.el so the two agree.
;;
;; The .project-locals.el file is read as a plain Lisp form and applied as
;; dir-locals at the project root.  See `project-compile.el' for the compile
;; command DSL that piggybacks on this mechanism.

;;; Code:

(use-package project
  :ensure nil
  :init
  ;; 1. Any directory with .project-locals.el is a project root.
  (defun my/project-try-local (dir)
    "Treat any directory containing .project-locals.el as a project root."
    (when-let ((root (locate-dominating-file dir ".project-locals.el")))
      (cons 'transient (expand-file-name root))))

  ;; Priority -90 runs BEFORE project-try-vc so a parent .git can't shadow
  ;; a closer .project-locals.el marker.
  (add-hook 'project-find-functions #'my/project-try-local -90)

  ;; 2. Make .project-locals.el behave like .dir-locals.el at the project root.
  (defun my/load-project-locals ()
    "Apply .project-locals.el at the project root as dir-locals."
    (when-let* ((file-or-dir (or buffer-file-name default-directory))
                (root (locate-dominating-file file-or-dir ".project-locals.el"))
                (file (expand-file-name ".project-locals.el" root))
                ((file-readable-p file)))
      (condition-case err
          (let* ((class (intern (concat "project-locals:" (expand-file-name root))))
                 (variables (with-temp-buffer
                              (insert-file-contents file)
                              ;; Guard against empty files — read would signal
                              ;; "End of file during parsing" on a zero-byte file.
                              (if (zerop (buffer-size))
                                  '()
                                (read (current-buffer))))))
            (dir-locals-set-class-variables class variables)
            (dir-locals-set-directory-class root class)
            (hack-dir-local-variables-non-file-buffer))
        (error (message "Failed to load %s: %s" file (error-message-string err))))))

  (add-hook 'find-file-hook #'my/load-project-locals)
  (add-hook 'dired-mode-hook #'my/load-project-locals))

;; Make lsp-mode use project.el for root detection so it agrees with the
;; project-find-functions chain above (in particular, .project-locals.el).
(with-eval-after-load 'lsp-mode
  (defun my/lsp-project-root-from-project-el (orig &rest args)
    (or (when-let ((proj (project-current)))
          (project-root proj))
        (apply orig args)))
  (advice-add 'lsp--calculate-root :around #'my/lsp-project-root-from-project-el))

(provide 'project-setup)
;;; project-setup.el ends here
