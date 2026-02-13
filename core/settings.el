;;; settings.el --- Basic settings for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; General GUI settings, common packages and dependencies, qol.

;;; Code:

;; ----------------------------
;; Basic GUI settings
;; ----------------------------
(menu-bar-mode 0)       ;; Disable the menu bar
(tool-bar-mode 0)       ;; Disable the toolbar
(scroll-bar-mode 0)     ;; Enables visible scrollbar
(tooltip-mode 0)        ;; Disable tooltips
(blink-cursor-mode 0)   ;; Disables cursor blinking
(set-fringe-mode 16)    ;; Fringe width

(setq visible-cursor nil)
(setq inhibit-startup-message t) ;; Do not show startup screen
(setq visible-bell nil)          ;; No visual bell

(use-package nerd-icons
  :ensure t
  :demand t)

(use-package modus-themes
  :ensure t
  :demand t
  ;; :config
  ;;(load-theme 'modus-vivendi t)
  ;; TODO TEMP
  ;;(set-face-background 'hl-line "#262626")
  ;; (set-face-background 'default unspecified)
  ;; (set-face-background 'line-number unspecified)
  )

(set-frame-parameter nil 'alpha-background 92)


;; ----------------------------
;; Other basic settings & built-in packages (or their replacements)
;; ----------------------------
(global-display-line-numbers-mode 1) ;; Shows line numbering globally
(global-hl-line-mode 1)              ;; Highlight the current line globally
(show-paren-mode 1)                  ;; Highlight matching
(fset 'yes-or-no-p 'y-or-n-p)        ;; Make `y` and `n` confirm instead of `yes` and `no`

(setq display-line-numbers-type 'visual)
(setq display-line-numbers-current-absolute nil)  ;; Show '0' for current line
(setq display-line-numbers-width 2)               ;; TODO: Fixed minimal width
(setq display-line-numbers-grow-only nil)         ;; Allow shrinking
(setq display-line-numbers-widen nil)             ;; prevents widening
(global-visual-line-mode 1)                       ;; Visual line mode for wrapped lines

;; TODO: possible move in the modes themselves
(defun display-line-numbers--turn-on ()
  "Turn on line numbers, but not in excluded modes."
  (unless (or (minibufferp)
              (derived-mode-p 'devdocs-mode))
    (display-line-numbers-mode 1)))

(setq auto-save-default t
      auto-save-timeout 20     ;; Auto-save after 20 seconds idle
      auto-save-interval 200)  ;; Auto-save after 200 keystrokes

(use-package which-key
  :ensure t
  :demand t
  :config
  (setq which-key-idle-delay 0.01)
  (which-key-mode 1))

(use-package rg
  :ensure t
  :config
  (rg-enable-menu))

(use-package autorevert
  :ensure nil
  :demand t
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode 1))

(use-package recentf
  :ensure nil
  :demand t
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100))

(use-package savehist
  :ensure nil
  :demand t
  :config
  (setq history-length 1000
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (savehist-mode 1))

(use-package transient
  :ensure t
  :demand t)

(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode -1)) 

;; ----------------------------
;; QOL packages
;; ----------------------------
(use-package winner
  :init
  (winner-mode t))

(use-package colorful-mode
  :ensure t
  :init
  ;; If non-nil, use prefix for preview color instead highlight them.)
  (setq colorful-use-prefix nil))

(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package hide-mode-line
  :ensure t)

(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; No-littering - keeps .emacs.d clean
(use-package no-littering
  :ensure t
  :demand t
  :config
  (no-littering-theme-backups)

  ;; Use ~/.local/emacs
  (setq no-littering-etc-directory "~/.local/emacs/etc/"
        no-littering-var-directory "~/.local/emacs/var/")

  ;; Auto-save files
  (setq auto-save-file-name-transforms
        `((".*" "~/.local/emacs/var/auto-save/" t)))

  ;; Backup files
  (setq backup-directory-alist
        `(("." . "~/.local/emacs/var/backup/")))

  ;; Lock files
  (setq lock-file-name-transforms
        `((".*" "~/.local/emacs/var/lock/" t)))

  (make-directory "~/.local/emacs/var/auto-save/" t)
  (make-directory "~/.local/emacs/var/backup/" t)
  (make-directory "~/.local/emacs/var/lock/" t)
  (make-directory "~/.local/emacs/var/undo-tree-hist/" t))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?w ?o ?f ?j ?e ?i ?d ?k ?l)))

(use-package avy
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map ("s" . avy-goto-char-2)))

;; TODO contract
(use-package expand-region
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map ("M-f" . er/expand-region)))

(use-package grid
  :ensure (:host github :repo "ichernyshovvv/grid.el")
  :demand t)

(provide 'settings)
;;; settings.el ends here

