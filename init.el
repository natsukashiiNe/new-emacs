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

;; Set elpaca to use ~/.emacs.d (like straight.el did)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" "~/.emacs.d/"))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
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

	  (error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; CRITICAL: Block until elpaca and use-package are ready
(elpaca-wait)

;; ==================================
;; Config Directory Setup
;; ==================================

(setq user-emacs-directory "~/.emacs.d/")
(defvar my-config-dir "~/.config/emacs/"
  "Directory that contains all the .el config files.")

(defun load-config-file (file)
  "Load an Emacs Lisp FILE from `my-config-dir`.
Also reports errors with file and line number."
  (let ((path (expand-file-name file my-config-dir)))
    (if (file-exists-p path)
        (condition-case err
            (progn
              (message "🔄 Loading: %s" file)
	      (load-file path)
              (message "✅ Successfully loaded: %s" file))
          (error
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
(add-to-list 'load-path (expand-file-name "org" my-config-dir))
(add-to-list 'load-path (expand-file-name "core" my-config-dir))
(add-to-list 'load-path (expand-file-name "code" my-config-dir))
(add-to-list 'load-path (expand-file-name "local" my-config-dir))
(add-to-list 'load-path (expand-file-name "custom" my-config-dir))
(add-to-list 'load-path (expand-file-name "custom/packages" my-config-dir))

(load-config-file "core/evil-settings.el")
(load-config-file "core/settings.el")
(elpaca-wait)

;; TODO: move to frame-settings
(load-theme 'my-modus-mono-dark t)
(defun reload-my-theme()
  (interactive)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" my-config-dir))
  (load-theme 'my-modus-mono-dark t))
(keymap-set global-map "C-c R" #'reload-my-theme)

(load-config-file "local/get-secrets.el")
(load-config-file "local/local-env.el")

(load-config-file "core/bar-settings.el")
;; TODO: remove test to an actual file
(load-config-file "core/dirvish-settings-test.el") 
(load-config-file "core/posframe-setup.el")
(load-config-file "core/completion-setup.el")
(load-config-file "core/session-settings.el")
(load-config-file "core/autocomplete-settings.el")
(load-config-file "core/better-built-in-modes.el")

(load-config-file "custom/packages/slot-layout.el")
(load-config-file "core/layout-settings.el")
(load-config-file "core/floating-layout.el")

(load-config-file "core/treemacs-settings.el")

(load-config-file "code/lsp-setup.el")        ;; Core lsp-mode + lsp-ui + formatting.
;; (load-config-file "code/docs-setup.el")    ;; Core lsp-mode + lsp-ui + formatting.
(load-config-file "code/lsp-servers.el")      ;; Language-specific server configs.
(load-config-file "code/lisp-editing.el")     ;; TODO: actual lisp
(load-config-file "code/git-settings.el")     ;; Core lsp-mode + lsp-ui + formatting.
(load-config-file "code/flycheck-setup.el")   ;; Diagnostics display (flycheck + flyover).
(load-config-file "code/snippets-setup.el")   ;; code/snippets stores snippet files.
(load-config-file "code/treesitter-setup.el")

(load-config-file "org/org-plugins.el")
(load-config-file "org/org-settings.el")

(load-config-file "custom/elgo.el")
(load-config-file "custom/custom-compile.el")

;; TODO: fix:
(load-config-file "refactor/general-keymaps.el")
(load-config-file "keymaps/mode-keymaps.el")

(message "🎉 Emacs startup complete!")

;; ==-- TODO: TEMP mappings --================================================================
(keymap-set global-map "C-x g"   'consult-ripgrep)
(keymap-set global-map "C-x F"   'consult-ls-git)

;; Note: M-j and M-k are defined in code/flycheck-setup.el
(elpaca-wait)

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
   ("q" "quit" transient-quit-one)])

(defun my/show-messages-below-window ()
  "Split current window and show messages in 30% below"
  (interactive)
  (let ((height (floor (* 0.3 (window-height)))))
    (split-window-below (- height))
    (other-window 1)
    (switch-to-buffer "*Messages*")))

(defun my/show-messages-below-frame ()
  "Split from frame bottom and show messages in 30%"
  (interactive)
  (let ((height (floor (* 0.3 (frame-height)))))
    (select-window (split-root-window-below (- height)))
    (switch-to-buffer "*Messages*")))

(global-set-key (kbd "C-x l") 'my/layout-menu)

(provide 'init)
;;; init.el ends here
