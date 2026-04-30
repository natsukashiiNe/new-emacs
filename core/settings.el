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
(setq require-final-newline nil)

(setq visible-cursor nil)
(setq inhibit-startup-message t) ;; Do not show startup screen
(setq visible-bell nil)          ;; No visual bell

(set-frame-parameter nil 'alpha '(92 . 92))

;; == Performance ===============================================================

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(global-so-long-mode 1)
(when (boundp 'long-line-threshold)
  (setq long-line-threshold 1000))

;; == Some utility packages =====================================================

(use-package nerd-icons
  :ensure t
  :demand t)

(use-package catppuccin-theme
  :ensure t)

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

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  ;; (("<f5>" . modus-themes-rotate)
  ;;  ("C-<f5>" . modus-themes-select)
  ;;  ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  ;;(modus-themes-load-theme 'ef-day)
  )

;; ----------------------------
;; Other basic settings & built-in packages (or their replacements)
;; ----------------------------
;; (global-display-line-numbers-mode t) ;; Shows line numbering globalle
(global-hl-line-mode -1)             ;; DO NOT Highlight the current line globally
(show-paren-mode 1)                  ;; Highlight matching
(fset 'yes-or-no-p 'y-or-n-p)        ;; Make `y` and `n` confirm instead of `yes` and `no`

;; (setq display-line-numbers-type 'visual)
(setq display-line-numbers-type 'visual)
(setq display-line-numbers-current-absolute nil)  ;; Show '0' for current line
(setq display-line-numbers-width 2)               ;; TODO: Fixed minimal width
(setq display-line-numbers-grow-only nil)         ;; Allow shrinking
(setq display-line-numbers-widen nil)             ;; prevents widening
(global-visual-line-mode -1)
(dolist (hook '(text-mode-hook
		org-mode-hook
		markdown-mode-hook
		help-mode-hook
		diff-mode-hook))
  (add-hook hook #'visual-line-mode))

(setq lazy-highlight-cleanup nil)           ;; persistent isearch colors
(setq lazy-highlight-buffer-max-at-a-time nil) ;; no per-cycle limit
(setq lazy-highlight-buffer t)                 ;; highlight across entire buffer


;; TODO: make it not suck
(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 35 35 :left :elide)
              " " (size 9 -1 :right)
              " " (mode 16 16 :left :elide)
              "\n" "      " filename-and-process)
        ;; Compact fallback — press ` to toggle
        (mark modified read-only locked
              " " (name 35 35 :left :elide)
              " " (size 9 -1 :right)
              " " (mode 16 16 :left :elide)
              " " filename-and-process)))

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
  (setq which-key-idle-delay 0.250)
  (which-key-mode 1))

(use-package rg
  :ensure t
  :defer t
  :config
  (rg-enable-menu))


(use-package fd-dired
  :ensure t
  :config
  (setq fd-dired-ls-option
	'("| xargs -0 ls -1d --quoting-style=literal" . "-1d")))

(use-package autorevert
  :ensure nil
  :demand t
  :config
  (setq global-auto-revert-non-file-buffers nil)  ; was t — causes constant dired revert & CPU spike
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

(use-package visual-regexp
  :ensure t
  :defer t)

(use-package visual-regexp-steroids
  :ensure t
  :defer t
  :after visual-regexp)


;; ----------------------------
;; QOL packages
;; ----------------------------
(use-package winner
  :init
  (winner-mode t))

(use-package command-log-mode
  :ensure t)

(use-package colorful-mode
  :ensure t
  :init
  ;; If non-nil, use prefix for preview color instead highlight them.)
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string "  "))

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
  ;; :bind (:map evil-normal-state-map ("s" . avy-goto-char-2))
  )

;; TODO contract
(use-package expand-region
  :ensure t
  :after evil
  ;;:bind (:map evil-normal-state-map ("M-f" . er/expand-region))
  )

(use-package grid
  :ensure (:host github :repo "ichernyshovvv/grid.el")
  :demand t)


(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :bind-keymap ("C-c O" . symbol-overlay-map)
  :custom
  (symbol-overlay-idle-time 0.1)
  (symbol-overlay-temp-highlight-single t))

(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode org-mode)
  :config
  (require 'smartparens-config))

(use-package highlight-indent-guides

  :ensure t
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-delay 0) 

  :config
  ;;  (set-face-foreground 'highlight-indent-guides-top-character-face "dimgray")
  ;;  (set-face-foreground 'highlight-indent-guides-top-even-face        "dimgray")
  ;;  (set-face-foreground 'highlight-indent-guides-top-odd-face "dimgray")

  ;;  (set-face-foreground 'highlight-indent-guides-even-face "dimgray")
  ;;  (set-face-foreground 'highlight-indent-guides-odd-face "dimgray")

  (set-face-foreground 'highlight-indent-guides-stack-character-face "#D7AF00")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  

  ;; (setq highlight-indent-guides-auto-stack-odd-face-perc       2)
  ;; (setq highlight-indent-guides-auto-stack-even-face-perc      2)
  ;; (setq highlight-indent-guides-auto-stack-top-character-face-perc 2)
  )

(use-package indent-bars
  :ensure t
  :hook ((prog-mode . indent-bars-mode)
         (lisp-mode . indent-bars-mode)
         (emacs-lisp-mode . indent-bars-mode))
  :init
  (defvar indent-bars-treesit-scope nil
    "Pre-declare so lang files can `add-to-list' before indent-bars-ts loads.")
  :custom
  (indent-bars-color-by-depth nil)  
  ;; base color
  (indent-bars-color '(highlight :face-bg t :blend 0.3))
  (indent-bars-highlight-current-depth '(:color "#7A6200" :blend 0.7))  ; active scope: orange/yellow

  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope-min-lines 3)
  (indent-bars-no-descend-string t)
  (indent-bars-no-descend-lists 'skip)
  (indent-bars-starting-column 0)
  (indent-bars-width-frac 0.15))     ; thin lines

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package consult-todo
  :ensure t
  :after (consult hl-todo)
  :demand t)

(provide 'settings)
;;; settings.el ends here
