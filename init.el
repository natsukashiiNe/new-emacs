;;; init.el --- Startup file -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs init file

;;; Code:

;; Enable native compilation if available
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors nil))

;; ==================================
;; Package Manager - Elpaca
;; ==================================

(setq package-enable-at-startup nil)

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Per-package load/config timing, viewable via `use-package-report'.
(setq use-package-compute-statistics t)

;; ==================================
;; Config Directory Setup
;; ==================================

(setq user-emacs-directory "~/.emacs.d/")
(defvar my-config-dir "~/.config/emacs/"
  "Directory that contains all the .el config files.")

(defvar my/config-load-times nil
  "Alist of (FILE . SECONDS) load duration for each `load-config-file' call.
Populated in load order; see `my/show-startup-profile' for a sorted report.")

(defun load-config-file (file)
  "Load an Emacs Lisp FILE from `my-config-dir`.
Also reports errors with file and line number, and records load
duration into `my/config-load-times'."
  (let ((path (expand-file-name file my-config-dir))
        (start (current-time)))
    (if (file-exists-p path)
        (condition-case err
            (progn
              (message "🔄 Loading: %s" file)
	      (load-file path)
              (push (cons file (float-time (time-subtract (current-time) start)))
                    my/config-load-times)
              (message "✅ Successfully loaded: %s" file))
          (error
           (push (cons file (float-time (time-subtract (current-time) start)))
                 my/config-load-times)
           (message "❌ ERROR in %s: %s" file (error-message-string err))
           (with-current-buffer "*Messages*"
             (goto-char (point-max))
             (re-search-backward (format "Error in file %s" file) nil t)
             (message "💡 See error details above for file: %s" file))))
      (message "⚠️ Warning: Config file %s not found" file))))

;; ==================================
;; Load Config Files
;; ==================================

(add-to-list 'custom-theme-load-path (expand-file-name "themes" my-config-dir))
(add-to-list 'load-path (expand-file-name "org"             my-config-dir))
(add-to-list 'load-path (expand-file-name "core"            my-config-dir))
(add-to-list 'load-path (expand-file-name "core/dired"      my-config-dir))
(add-to-list 'load-path (expand-file-name "code"            my-config-dir))
(add-to-list 'load-path (expand-file-name "code/lsp"        my-config-dir))
(add-to-list 'load-path (expand-file-name "code/lsp/language-settings/" my-config-dir))
(add-to-list 'load-path (expand-file-name "code/lsp/language-settings/web/" my-config-dir))
(add-to-list 'load-path (expand-file-name "code/flymake"    my-config-dir))
(add-to-list 'load-path (expand-file-name "local"           my-config-dir))
(add-to-list 'load-path (expand-file-name "custom"          my-config-dir))
(add-to-list 'load-path (expand-file-name "custom/packages" my-config-dir))
(add-to-list 'load-path (expand-file-name "keymaps"         my-config-dir))
(add-to-list 'load-path (expand-file-name "refactor"        my-config-dir))

(load-config-file "local/custom-macro.el")
;; (load-config-file "local/local-env.el")
(load-config-file "local/setup-local-env.el")
(load-config-file "local/setup-local-variables.el")
(load-config-file "core/evil-settings.el")
(elpaca-wait)

(load-config-file "core/settings.el")
(load-config-file "core/gui-settings.el")
(load-config-file "custom/custom-editing.el")
(load-config-file "custom/image-clipboard.el")
(load-config-file "keymaps/mode-keymaps.el")

;; MOVE to custom/
(load-config-file "local/get-secrets.el")

(load-config-file "core/ego-setup.el")
(load-config-file "core/bar-settings.el")
(load-config-file "core/dired/dirvish-setup.el")
(load-config-file "core/posframe-setup.el")
(load-config-file "core/completion-setup.el")
(load-config-file "core/session-settings.el")
(load-config-file "core/autocomplete-settings.el")
(load-config-file "core/better-built-in-modes.el")

;; (load-config-file "custom/packages/slot-layout.el")
;; (load-config-file "core/layout-settings.el")
(load-config-file "core/floating-layout.el")

;; (load-config-file "core/treemacs-settings.el")

(load-config-file "core/project/project-setup.el")   ;; project.el: .project-locals.el root marker + lsp root sync.
(load-config-file "core/project/project-compile.el") ;; Declarative per-project compile commands.
(load-config-file "code/lsp/lsp-setup.el")    ;; Core lsp-mode + lsp-ui + formatting.
(load-config-file "code/docs-setup.el")       ;; Core lsp-mode + lsp-ui + formatting.
(load-config-file "code/lisp-editing.el")     ;; TODO: actual lisp
(load-config-file "code/git-settings.el")     ;; Core lsp-mode + lsp-ui + formatting.
;; (load-config-file "code/flycheck-setup.el")   ;; OLD: Diagnostics display (flycheck + flyover).
(load-config-file "code/flymake/test-flymake-setup.el")   ;; FLYMAKE built-in.
(load-config-file "code/snippets-setup.el")   ;; code/snippets stores snippet files.
(load-config-file "code/treesitter-setup.el")
(load-config-file "code/code-utilities-setup.el")

(load-config-file "org/org-plugins.el")
(load-config-file "org/org-settings.el")

;; CUSTOM
(load-config-file "custom/elgo.el")
(load-config-file "custom/scripts-org.el")
;;(load-config-file "custom/packages/elastic.el")
(load-config-file "custom/packages/old-elastic.el")

;; APPS
(load-config-file "apps/telega-setup.el")
(load-config-file "apps/agent-shell-setup.el")
(load-config-file "apps/minuet-ai-setup.el")

;; TODO: fix:
(load-config-file "refactor/general-keymaps.el")

(message "🎉 Emacs startup complete!")

;; ==================================
;; Startup Profiling
;; ==================================
;; Two complementary views:
;; - `my/config-load-times' (per-file, populated by `load-config-file' above)
;;   shows which top-level config files are slow to load.
;; - `use-package-report' (per-package, via `use-package-compute-statistics'
;;   set near the Elpaca bootstrap) shows which individual packages inside
;;   those files spend time in :init/:config, and whether they were loaded
;;   eagerly or deferred.
;; Run `my/show-startup-profile' or `use-package-report' after startup.

(defun my/show-startup-profile ()
  "Show per-file config load times, slowest first, absolute and relative.
Complements `use-package-report', which breaks costs down per package
instead of per config file."
  (interactive)
  (let* ((sorted (sort (copy-sequence my/config-load-times)
                       (lambda (a b) (> (cdr a) (cdr b)))))
         (sum (apply #'+ (mapcar #'cdr my/config-load-times))))
    (with-current-buffer (get-buffer-create "*Startup Profile*")
      (erase-buffer)
      (insert (format "Emacs init time (reported by emacs-init-time): %s\n"
                      (emacs-init-time)))
      (insert (format "Sum of measured load-config-file calls:        %.3fs\n" sum))
      (insert (format "GC runs: %d   GC time: %.3fs\n\n" gcs-done gc-elapsed))
      (insert (format "%-50s %10s %8s\n" "File" "Seconds" "% sum"))
      (insert (make-string 71 ?-) "\n")
      (dolist (entry sorted)
        (insert (format "%-50s %10.4f %7.1f%%\n"
                        (car entry) (cdr entry)
                        (if (> sum 0) (* 100 (/ (cdr entry) sum)) 0))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer "*Startup Profile*")))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Startup profile: %s — M-x my/show-startup-profile (per-file) or M-x use-package-report (per-package)"
                     (emacs-init-time))))

;; ---- File logging, for batch/hyperfine benchmark runs -----------------------
;; A throwaway `emacs --eval "(run-with-timer ...)"' process (as used by
;; hyperfine) is killed before you can inspect its variables interactively.
;; When EMACS_STARTUP_PROFILE_LOG is set, append the same per-file and
;; per-package data to that file instead, one block per run. Unset by
;; default so ordinary daily startups don't write to disk.
(defvar my/startup-profile-log-file (getenv "EMACS_STARTUP_PROFILE_LOG")
  "When non-nil, append each startup's profile to this file.
Set via the EMACS_STARTUP_PROFILE_LOG environment variable rather than
here, so a single benchmark invocation can opt in without editing this
file.")

(defun my/format-startup-profile ()
  "Return the per-file and per-package startup profile as a string."
  (with-temp-buffer
    (insert (format "=== Startup profile %s (pid %d) ===\n"
                    (format-time-string "%Y-%m-%dT%H:%M:%S")
                    (emacs-pid)))
    (insert (format "emacs-init-time: %s\n" (emacs-init-time)))
    (insert (format "GC runs: %d   GC time: %.3fs\n\n" gcs-done gc-elapsed))
    (let* ((sorted (sort (copy-sequence my/config-load-times)
                         (lambda (a b) (> (cdr a) (cdr b)))))
           (sum (apply #'+ (mapcar #'cdr my/config-load-times))))
      (insert (format "-- Per-file (sum %.3fs) --\n" sum))
      (dolist (entry sorted)
        (insert (format "%-50s %10.4f %7.1f%%\n"
                        (car entry) (cdr entry)
                        (if (> sum 0) (* 100 (/ (cdr entry) sum)) 0)))))
    (insert "\n")
    (when (and (bound-and-true-p use-package-compute-statistics)
               (> (hash-table-count use-package-statistics) 0))
      (insert "-- Per-package (use-package) --\n")
      (let (rows)
        (maphash (lambda (pkg stats)
                   (push (list (symbol-name pkg)
                               (or (use-package-statistics-status stats) "?")
                               (use-package-statistics-time stats))
                         rows))
                 use-package-statistics)
        (setq rows (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b)))))
        (dolist (row rows)
          (insert (format "%-35s %-12s %8.4f\n" (nth 0 row) (nth 1 row) (nth 2 row))))))
    (insert "\n")
    (buffer-string)))

(defun my/log-startup-profile ()
  "Append the startup profile to `my/startup-profile-log-file' if set."
  (when my/startup-profile-log-file
    (write-region (my/format-startup-profile) nil
                  my/startup-profile-log-file 'append)))

(add-hook 'emacs-startup-hook #'my/log-startup-profile)

;; ==-- TODO: TEMP mappings --================================================================
(keymap-set global-map "C-x g"   'consult-ripgrep)
(keymap-set global-map "C-x F"   'consult-ls-git)

(with-eval-after-load 'transient
  (transient-define-prefix my/layout-menu ()
    "Window layout and resizing menu"
    ["Resize (stay in menu)"
     [("h" "enlarge right" enlarge-window-horizontally :transient t)
      ("H" "shrink right" shrink-window-horizontally :transient t)
      ("l" "enlarge left" shrink-window-horizontally :transient t)
      ("L" "shrink left" enlarge-window-horizontally :transient t)]
     [("j" "enlarge down" enlarge-window :transient t)
      ("J" "shrink down" shrink-window :transient t)
      ("k" "enlarge up" shrink-window :transient t)
      ("K" "shrink up" enlarge-window :transient t)]]
    ["Actions (exit menu)"
     ("m" "messages 30% of window" my/show-messages-below-window)
     ("M" "messages 30% of frame" my/show-messages-below-frame)
     ("q" "quit" transient-quit-one)]))

(defun my/show-messages-below-window ()
  "Split current window and show messages in 30% below."
  (interactive)
  (let ((height (floor (* 0.3 (window-height)))))
    (split-window-below (- height))
    (other-window 1)
    (switch-to-buffer "*Messages*")))

(defun my/show-messages-below-frame ()
  "Split from frame bottom and show messages in 30%."
  (interactive)
  (let ((height (floor (* 0.3 (frame-height)))))
    (select-window (split-root-window-below (- height)))
    (switch-to-buffer "*Messages*")))

(global-set-key (kbd "C-x l") 'my/layout-menu)

(provide 'init)
;; init.el ends here
