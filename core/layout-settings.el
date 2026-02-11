;;; layout-settings.el --- Window layouts using SLL -*- lexical-binding: t; -*-

;;; Commentary:
;; Layout definitions using the slot-layout (SLL) package.
;; Defines layouts with sublayouts for quick window combinations.

;;; Code:

(require 'slot-layout)
(require 'sll-persp)

;;; ============================================================================
;;; Layout Definitions
;;; ============================================================================

;; Simple development layout with bottom terminal
(sll-define-layout dev-terminal
  "Simple layout with bottom terminal."

  (sll-slots terminal)

  (sll-slot terminal
    :split (frame 'down 0.35)
    :modes (vterm-mode comint-mode eshell-mode compilation-mode
		       shell-mode flycheck-error-list-mode)
    :names ("*Messages*" "*Warnings*" "*compilation*"
            "*Backtrace*" "*Embark Export*")
    :regexps ("^\\*vterm" "^\\*eshell" "^\\*Embark Collect")
    :default-command vterm)

  ;; Sublayouts
  (sll-sublayouts code-only)
  (sll-sublayout code-only))  ;; Just MAIN, no terminal

;; Three-pane development layout
(sll-define-layout dev-3pane
  "Three-pane layout with terminal and sidebars."

  (sll-slots terminal sidebar)

  (sll-slot terminal
    :split (MAIN 'down 0.35)
    :modes (vterm-mode comint-mode eshell-mode compilation-mode
		       comint-mode shell-mode flycheck-error-list-mode)
    :names ("*Messages*" "*Warnings*" "*compilation*"
            "*Backtrace*" "*Embark Export*")
    :regexps ("^\\*vterm" "^\\*eshell" "^\\*Embark Collect")
    :default-command vterm)

  (sll-slot sidebar
    :split (frame 'left 0.22)
    :modes (dired-mode treemacs-mode)
    :regexps ("^\\*Treemacs")
    :default-command projectile-dired)

  ;; Sublayouts
  (sll-sublayouts term sidebar code-only)

  ;; Just terminal (for focused coding with REPL)
  (sll-sublayout term
    (terminal . vterm))

  ;; Sidebars only (file browsing + help)
  (sll-sublayout sidebar
    (sidebar . projectile-dired))

  ;; Just MAIN (distraction-free)
  (sll-sublayout code-only))

;; === ULTRA-WIDE Monitor ============================================================================
(sll-define-layout dev-3pane-uw
  "Three-pane layout with terminal and sidebars."

  (sll-slots terminal sidebar)

  (sll-slot terminal
    :split (MAIN 'down 0.27)
    :modes (vterm-mode comint-mode eshell-mode compilation-mode
		       comint-mode shell-mode flycheck-error-list-mode)
    :names ("*Messages*" "*Warnings*" "*Backtrace*"
            "*compilation*"
	    "*Embark Actions*" "*Embark Export*" )
    :regexps ("^\\*vterm" "^\\*eshell" "^\\*Embark Collect" )
    :default-command vterm)

  (sll-slot sidebar
    :split (frame 'left 0.30)
    :modes (dired-mode treemacs-mode)
    :regexps ("^\\*Treemacs")
    :default-command projectile-dired)

  ;; Sublayouts
  (sll-sublayouts term sidebar code-only)

  ;; Just terminal (for focused coding with REPL)
  (sll-sublayout term
    (terminal . vterm))

  ;; Sidebars only (file browsing + help)
  (sll-sublayout sidebar
    (sidebar . projectile-dired))

  ;; Just MAIN (distraction-free)
  (sll-sublayout code-only))

;; Minimal layout with help sidebar
(sll-define-layout minimal
  "Minimal layout with just a help sidebar."

  (sll-slots help)

  (sll-slot help
    :split (MAIN 'right 0.35)
    :modes (help-mode helpful-mode Info-mode)
    :names ("*Help*" "*info*")
    :default-command nil))

;;; ============================================================================
;;; Keybindings
;;; ============================================================================

;; Layout management
(keymap-set global-map "C-c l L" #'sll-load-layout)
(keymap-set global-map "C-c l s" #'sll-switch-layout)
(keymap-set global-map "C-c l l" #'sll-switch-sublayout)
(keymap-set global-map "C-c l r" #'sll-reset-layout)
(keymap-set global-map "C-c l q" #'sll-unload-layout)
(keymap-set global-map "C-c l d" #'sll-describe-layout)

;; Slot navigation
(keymap-set global-map "C-c l g" #'sll-switch-to-slot)
(keymap-set global-map "C-c l m" #'sll-send-buffer-to-slot)
(keymap-set global-map "C-c l n" #'sll-set-next-slot)
(keymap-set global-map "C-c l x" #'sll-execute-in-slot)

;; Slot visibility
(keymap-set global-map "C-c l t" #'sll-toggle-slot)
(keymap-set global-map "C-c l h" #'sll-hide-slot)
(keymap-set global-map "C-c l o" #'sll-show-slot)

;; Quick slot toggles
(defun my/toggle-terminal ()
  "Toggle terminal slot if available."
  (interactive)
  (if (memq 'terminal (sll-available-slots))
      (sll-toggle-slot 'terminal)
    (user-error "No terminal slot in current layout")))

(defun my/toggle-sidebar ()
  "Toggle sidebar-top slot if available."
  (interactive)
  (if (memq 'sidebar-top (sll-available-slots))
      (sll-toggle-slot 'sidebar-top)
    (user-error "No sidebar-top slot in current layout")))

(defun my-sll/switch-to-main ()
  "Switch to the main slot."
  (interactive)
  (sll-switch-to-slot 'MAIN))

(defun my-sll/switch-to-terminal ()
  "Switch to the main slot."
  (interactive)
  (sll-switch-to-slot 'terminal))

(defun my-sll/switch-to-sidebar ()
  "Switch to the main slot."
  (interactive)
  (sll-switch-to-slot 'sidebar))


(with-eval-after-load 'evil
  (evil-define-key 'normal evil-window-map
    (kbd "C-o") #'my-sll/switch-to-main
    (kbd "C-l") #'my-sll/switch-to-terminal
    (kbd "C-h") #'my-sll/switch-to-sidebar)
  (keymap-set evil-window-map "C-o" #'my-sll/switch-to-main)
  (keymap-set evil-window-map "C-l" #'my-sll/switch-to-terminal)
  (keymap-set evil-window-map "C-h" #'my-sll/switch-to-sidebar))


(keymap-set global-map "C-c l T" #'my/toggle-terminal)
(keymap-set global-map "C-c l S" #'my/toggle-sidebar)

;; Quick sublayout switches
(defun my-sll/sublayout-full ()
  "Switch to full sublayout."
  (interactive)
  (sll-switch-sublayout 'full))

(defun my-sll/sublayout-code-only ()
  "Switch to code-only sublayout."
  (interactive)
  (if (memq 'code-only (sll-available-sublayouts))
      (sll-switch-sublayout 'code-only)
    (user-error "No code-only sublayout")))


(keymap-set global-map "C-c l 1" #'my-sll/sublayout-code-only)
(keymap-set global-map "C-c l 0" #'my-sll/sublayout-full)

;;; ============================================================================
;;; Perspective Integration
;;; ============================================================================

;; Enable perspective-local layouts
;; Each perspective maintains its own SLL layout state
(if (bound-and-true-p persp-mode)
    (sll-persp-mode 1)
  (with-eval-after-load 'perspective
    (sll-persp-mode 1)))

;;; ============================================================================
;;; Optional: Auto-load layout on startup
;;; ============================================================================

;; Uncomment to auto-load a layout:
;; (add-hook 'emacs-startup-hook (lambda () (sll-load-layout 'dev-3pane)))

(provide 'layout-settings)
;;; layout-settings.el ends here
