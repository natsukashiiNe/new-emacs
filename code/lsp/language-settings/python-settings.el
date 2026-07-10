;;; python-settings.el --- Settings for python-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Python language configuration: pyright LSP, tree-sitter mode,
;; and rebinding of python-ts-mode-map actions under C-c h prefix.

;;; Code:

;; --- Tree-sitter mode ---
(use-package python-ts-mode
  :ensure nil  ; built-in
  :mode "\\.py\\'"
  :config
  (setq python-indent-offset 4))

(use-package py-isort
  :ensure t)

;; --- Apheleia formatter ---
;; ruff handles formatting + import sorting (replaces black + isort).
;; For projects without ruff, use .dir-locals.el:
;;   ((python-mode . ((apheleia-formatter . (isort black)))))
(with-eval-after-load 'apheleia
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff))

;; --- LSP Server ---
(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-auto-search-paths t)
  (lsp-pyright-diagnostic-mode "openFiles")
  (lsp-pyright-type-checking-mode "basic")
  :hook ((python-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)))

;; --- Rebind python-ts-mode-map under C-c h prefix ---
;; Python's default bindings pollute C-c C-* which we use for global maps.
;; Wipe the entire keymap, then build only C-c h with python actions.
(with-eval-after-load 'python
  ;; Nuke all default bindings
  (setcdr python-ts-mode-map nil)

  ;; Rebuild under C-c h
  (keymap-set python-ts-mode-map "C-c h"
              (define-keymap
                ;; shell / eval
                "b"   #'python-shell-send-block
                "c"   #'python-shell-send-buffer
                "d"   #'python-describe-at-point
                "e"   #'python-shell-send-statement
                "f"   #'python-eldoc-at-point
                "h"   #'imenu
                "l"   #'python-shell-send-file
                "p"   #'run-python
                "r"   #'python-shell-send-region
                "S"   #'python-shell-send-string
                "v"   #'python-check
                "x"   #'python-shell-send-defun
                "z"   #'python-shell-switch-to-shell
                ;; indent
                "<"   #'python-indent-shift-left
                ">"   #'python-indent-shift-right
                ;; imports
                "i a" #'python-add-import
                "i f" #'python-fix-imports
                "i r" #'python-remove-import
                "i s" #'python-sort-imports
                ;; skeletons
                "s c" #'python-skeleton-class
                "s d" #'python-skeleton-def
                "s f" #'python-skeleton-for
                "s i" #'python-skeleton-if
                "s m" #'python-skeleton-import
                "s t" #'python-skeleton-try
                "s w" #'python-skeleton-while)))

(with-eval-after-load 'slime-py
  (evil-define-key '(normal insert) python-ts-mode-map
    (kbd "C-x C-e") #'slime-eval-last-expression)
  (evil-define-key 'visual python-ts-mode-map
    (kbd "C-x C-e") #'slime-eval-region))


(with-eval-after-load 'python
  (which-key-add-keymap-based-replacements python-ts-mode-map
    "C-c h s" "skeletons"
    "C-c h i" "imports"))


;; == REPL-based Python dev =====================================================
;; ---- Swanky-python: SLIME based python repl.

;; https://codeberg.org/sczi/swanky-python/
;; slime-py.el lives in the slimy-python/ subdirectory of the repo.
;; :files flattens the glob into the build root; :main gives the full
;; relative path so Elpaca's dependency scanner finds the file.
(use-package slime-py
  :ensure (:host codeberg :repo "sczi/swanky-python"
		 :files ("slimy-python/*.el" "slimy-python/snakes.txt")
		 :main "slimy-python/slime-py.el")
  :after (slime slime-star py-isort))


(with-eval-after-load 'slime-py
  (add-to-list 'slime-contribs 'slime-py)

  (defun my/slime-python--non-home-root (marker)
    "Find MARKER above `default-directory', ignoring the home directory."
    (let ((root (locate-dominating-file default-directory marker))
          (home-root (file-name-as-directory (expand-file-name "~"))))
      (unless (and root
                   (equal (file-name-as-directory (expand-file-name root))
                          home-root))
        root)))

  (defun my/slime-python-project ()
    "Select the Python runtime for Swanky Python.

Prefer a project-local .venv, then an explicit uv project, then the
dedicated scratch SLIME environment.  Avoid falling back to system
python3, because it usually will not have swanky_python installed."
    (let* ((uv-root (my/slime-python--non-home-root "uv.lock"))
           (venv-root (my/slime-python--non-home-root ".venv"))
           (project-root (or uv-root
                             (my/slime-python--non-home-root "pyproject.toml")
                             (my/slime-python--non-home-root "requirements.txt")
                             (my/slime-python--non-home-root "setup.py")
                             (my/slime-python--non-home-root ".dir-locals.el")
                             (my/slime-python--non-home-root ".project-locals.el")
                             venv-root))
           (venv-python (and venv-root
                             (expand-file-name ".venv/bin/python" venv-root)))
           (scratch-python (expand-file-name "~/.venvs/slime-python/bin/python")))
      (cond
       ((and venv-python (file-executable-p venv-python))
        `((,venv-python) :directory ,project-root))
       (uv-root
        `(("uv" "run" "python") :directory ,uv-root))
       ((file-executable-p scratch-python)
        `((,scratch-python) :directory ,(or project-root default-directory)))
       (t
        (error "No project .venv and no executable ~/.venvs/slime-python/bin/python")))))

  (add-to-list 'slime-lisp-implementations
               '(project-python my/slime-python-project))

  (defun my/slime-py-set-default-directory (directory)
    "Set Swanky Python's current directory without importing current module.

The stock SLIME command sends the request using `slime-current-package'.
For a Python source buffer that package is the filename, and Swanky may
import that module before handling the directory change.  That is bad for
files with top-level relative IO, so send this administrative request from
__main__ instead."
    (interactive (list (read-directory-name "Directory: " nil nil t)))
    (let ((dir (expand-file-name directory)))
      (message "default-directory: %s"
               (slime-from-lisp-filename
                (slime-repl-shortcut-eval
                 `(swank:set-default-directory ,(slime-to-lisp-filename dir))
                 "__main__")))
      (with-current-buffer (slime-output-buffer)
        (setq default-directory dir))
      (when-let* ((proc (slime-process))
                  (buffer (process-buffer proc)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq default-directory dir))))))

  (defun my/slime-py-sync-package-and-default-directory ()
    "Sync Python REPL module and cwd to the current source buffer.

Set cwd first using `my/slime-py-set-default-directory', then set the
REPL package/module.  This avoids importing the source module while the
Python process is still in an unrelated directory."
    (interactive)
    (let ((package (slime-current-package))
          (directory default-directory))
      (my/slime-py-set-default-directory directory)
      (when package
        (slime-repl-set-package package))
      (message "package: %s  directory: %s"
               (with-current-buffer (slime-output-buffer)
                 (slime-lisp-package))
               directory)))

  (advice-add 'slime-set-default-directory
              :override #'my/slime-py-set-default-directory)
  (advice-add 'slime-sync-package-and-default-directory
              :override #'my/slime-py-sync-package-and-default-directory)

  (defun my/slime-py-snake-fallback (oldfun &rest args)
    "Use source-tree snakes.txt if Elpaca build omitted the banner asset."
    (condition-case nil
        (apply oldfun args)
      (file-missing
       (let ((source-snakes
              (expand-file-name
               "slimy-python/snakes.txt"
               "/home/nane/.emacs.d/elpaca/sources/swanky-python/")))
         (if (file-readable-p source-snakes)
             (thread-first
               (with-temp-buffer
                 (insert-file-contents source-snakes)
                 (buffer-string))
               (string-split "\n\n")
               seq-random-elt
               insert)
           (insert "Swanky Python\n")))
       (insert "\n"))))

  (advice-add 'slime-py-snake :around #'my/slime-py-snake-fallback)

  (setq slime-default-lisp 'project-python)

  (slime-setup))

(provide 'python-settings)
;;; python-settings.el ends here
