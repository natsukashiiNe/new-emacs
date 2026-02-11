;;; better-built-in-modes.el --- Better built-in modes for Emacs. -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhancments for built-in modes.

;;; Code:

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

;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (dirvish-override-dired-mode)
;;   :config
;;   ;; Enable preview on the side
;;   (setq dirvish-mode-line-format
;;         '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
;;   (setq dirvish-preview-side 'right)  ; or 'left
;;   (setq dirvish-preview-size 0.5)  ; preview is 50% of frame width

;;   ;; Enable file type dispatchers for preview
;;   (setq dirvish-preview-dispatchers
;;         '(image gif video audio epub archive pdf))

;;   ;; Attributes to show (including preview)
;;   (setq dirvish-attributes
;;         '(nerd-icons file-info file-size collapse subtree-state vc-state git-msg)) 

;;   (setq dirvish-default-layout '(0 0.4 0.6))  ; (window-min-height left-width right-width)

;;   :bind
;;   (:map dirvish-mode-map
;; 	("TAB" . dirvish-toggle-preview)     ; Toggle preview on/off
;; 	("SPC" . dirvish-show-history)
;; 	("b"   . dirvish-goto-bookmark)
;; 	("z"   . dirvish-history-jump)
;; 	("f"   . dirvish-fd-jump)            ; Use fd to jump to file
;; 	("s"   . dirvish-quicksort)
;; 	("y"   . dirvish-yank)
;; 	("h"   . dired-up-directory)
;; 	("l"   . dired-find-file)
;; 	("a"   . dirvish-quick-access)
;; 	("q"   . dirvish-quit)))

;; Undo-tree with history in ~/.local/emacs/var/undo-tree-hist
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        '(("." . "~/.local/emacs/var/undo-tree-hist/"))))

(use-package vdiff
  :ensure t
  :init
  (setq vterm-always-compile-module t)
  :config
  (keymap-set global-map "C-c t" 'vterm))
  

(use-package vterm
  :ensure t
  :config

  ;; keymaps
  (with-eval-after-load 'evil
    (evil-define-key 'insert vterm-mode-map (kbd "C-h") #'vterm-send-backspace))
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  (vterm-mode . (lambda () (hl-line-mode -1)))
  )

(use-package multi-vterm
  :ensure t
  :after vterm)

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
