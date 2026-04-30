;;; better-built-in-modes.el --- Better built-in modes for Emacs. -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhancments for built-in modes.

;;; Code:

(use-package isearch-mb
  :ensure t
  :config
  (isearch-mb-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;; TODO fix this
;; Error (use-package): Failed to parse package casual: Wrong type argument: sequencep, t
(use-package casual
  :ensure t
  :bind
  (:map dired-mode-map ("C-o" . casual-dired-tmenu))
  (:map calc-mode-map  ("C-o" . casual-calc-tmenu)))

;; Undo-tree with history in ~/.local/emacs/var/undo-tree-hist
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        '(("." . "~/.local/emacs/var/undo-tree-hist/"))))

(use-package vdiff
  :ensure t)

;; =============================================================================
;; TERMINALS
;; =============================================================================

(use-package vterm
  :ensure t
  :init
  (setq vterm-always-compile-module t)
  (setenv "INSIDE_EMACS_VTERM" "1")
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  (vterm-mode . (lambda () (hl-line-mode -1)))
  (vterm-mode . my/vterm-set-modeline)
  :config
  (defun my/vterm--evil-state-face ()
    "Return the doom-modeline face for the current evil state."
    (pcase evil-state
      ('normal  'doom-modeline-evil-normal-state)
      ('insert  'doom-modeline-evil-insert-state)
      ('visual  'doom-modeline-evil-visual-state)
      ('replace 'doom-modeline-evil-replace-state)
      ('motion  'doom-modeline-evil-motion-state)
      ('emacs   'doom-modeline-evil-emacs-state)
      ('operator 'doom-modeline-evil-operator-state)
      (_        'doom-modeline-evil-normal-state)))

  (defun my/vterm-set-modeline ()
    "Set a minimal modeline showing [evil-state] [pwd]."
    (setq-local mode-line-format
                '(" "
                  (:eval (propertize (concat " " (upcase (symbol-name evil-state)) " ")
                                     'face (my/vterm--evil-state-face)))
                  "  "
                  default-directory)))
  ;; keymaps
  (with-eval-after-load 'evil
    (evil-define-key 'insert vterm-mode-map (kbd "C-h") #'vterm-send-backspace)))

(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  ;; TODO: use-last (toggle / tab / float) vs create new.
  (keymap-set global-map "C-c t" 'multi-vterm))

;; =============================================================================
;; COMPILATION
;; =============================================================================

(use-package fancy-compilation
  :ensure t
  :commands (fancy-compilation-mode)
  :init
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

;; Multi-compilation configurations
(use-package compile-multi
  :ensure t
  :commands (compile-multi)
  :config
  (setq compile-multi-config
        '((cmake-release . ("cmake --preset conan-release && cmake --build --preset conan-release -j8"))
          (cmake-debug . ("cmake --preset conan-debug && cmake --build --preset conan-debug -j8"))
          (clean . ("rm -rf build"))
          (test . ("ctest --preset conan-release")))))

;; TODO move to compilation 
(with-eval-after-load 'consult
  (require 'consult-compile nil t))

;; =============================================================================
;; FIX: some workarounds
;; =============================================================================

(require 'ansi-color)
(require 'mode-local)

(setq compilation-scroll-output t
      compilation-environment '("TERM=xterm-256color"))

;; Scroll behavior in compilation buffers
(dolist (sym '(scroll-margin scroll-conservatively scroll-preserve-screen-position))
  (put sym 'permanent-local t))

(setq-mode-local compilation-mode
		 scroll-margin 0
		 scroll-conservatively 101
		 scroll-preserve-screen-position 'always
		 next-error-recenter nil
		 display-line-numbers nil)

;; TODO: Clean output (probably there a package for that)
(defun my/compilation-colorize-and-cleanup ()
  "Strip OSC hyperlinks and colorize compilation buffer."
  (when (derived-mode-p 'compilation-mode)
    (let ((inhibit-read-only t))
      ;; Strip OSC 8 hyperlinks: ESC]8;;...BEL or ESC]8;;...ESC\
      (save-excursion
        (goto-char compilation-filter-start)
        (while (re-search-forward "\e\\]8;;[^\a\e]*\\(?:\a\\|\e\\\\\\)" nil t)
          (replace-match "")))
      ;; Apply ANSI colors
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(defun my/compilation-fix-background ()
  "Ensure compilation buffer uses theme background."
  (when (derived-mode-p 'compilation-mode)
    ;; Get actual background color from theme
    (let ((bg (face-attribute 'default :background nil 'default)))
      (unless (eq bg 'unspecified)
        (face-remap-add-relative 'default :background bg)))
    ;; Ensure line numbers are disabled
    (display-line-numbers-mode -1)))

(add-hook 'compilation-filter-hook #'my/compilation-colorize-and-cleanup)
(add-hook 'compilation-mode-hook #'my/compilation-fix-background)

(provide 'better-built-in-modes)
;;; better-built-in-modes.el ends here
