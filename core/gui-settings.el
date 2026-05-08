;;; gui-settings.el --- Settings for GUI/TTY frames. -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains settings of interface.
;; Settings in this files would be applied to newly created frames.
;; It can depend on whether Emacs is running in GUI or TTY modes.
;; Describes themes, frame parameters, bar settings, dynamically changing faces.


;;; Code:

;; == FRAME SETTINGS ======================================================

(load-theme 'my-modus-mono-dark t)
(set-face-attribute 'default nil :font "GoMono Nerd Font-21")
(set-face-attribute 'variable-pitch nil :font "GoMono Nerd Font-20")

(defun my/theme-disable-theme ()
  (mapc #'disable-theme (custom-available-themes) ))

(defun my/theme-reload-my-theme()
  (interactive)
  (let ((theme (car custom-enabled-themes)))
    (my/theme-disable-theme)
    (add-to-list 'custom-theme-load-path (expand-file-name "themes" my-config-dir))
    (load-theme theme t)))
;; TODO: move to INTERFACE theme (C-c u) + toggling of terminal, divish, flycheck, resizing etc.
(keymap-set global-map "C-c R" #'my/theme-reload-my-theme)

(defun my/setup-tty-frame (&optional frame)
  "Apply settings for TTY frames."
  (let ((f (or frame (selected-frame))))
    (with-selected-frame f
      (when (not (display-graphic-p f))
        (set-face-attribute 'default f :background "unspecified-bg")))))

(defun my/setup-gui-frame (&optional frame)
  "Apply settings for GUI frames."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p frame)
      (set-fringe-mode 16)    ;; Fringe width
      (set-face-attribute 'default frame :font "GoMono Nerd Font-21")
      (set-face-attribute 'variable-pitch frame :font "GoMono Nerd Font-20")
      (set-frame-parameter frame 'alpha '(92 . 92)))))

(defun my/setup-frame (&optional frame)
  "Apply appropriate settings based on frame type."
  (if (display-graphic-p frame)
      (my/setup-gui-frame frame)
    (my/setup-tty-frame frame)))

(unless (daemonp)
  (my/setup-frame))

(add-hook 'elpaca-after-init-hook
          (lambda () (unless (daemonp) (my/setup-frame))))
(add-hook 'after-make-frame-functions #'my/setup-frame)

;; == SHR =======================================================================
(defun my/shr-buffer-font ()
  (when (display-graphic-p)
    (face-remap-add-relative 'default :family "GoMono Nerd Font" :height 210)
    (face-remap-add-relative 'variable-pitch :family "GoMono Nerd Font")))

(add-hook 'eww-after-render-hook #'my/shr-buffer-font)
(add-hook 'devdocs-mode-hook #'my/shr-buffer-font)


;; == TAB BAR ========================================================

;; == MODE LINE ======================================================

;; == CUSTOMS ===================================================================


(defun my-gui/switch-ui (mode)
  "Switch UI between zen (hide bars) and normal (show bars).
MODE is either \\='zen or \\='normal."
  (interactive
   (list (intern (completing-read "UI mode: " '("zen" "normal") nil t))))
  (pcase mode
    ('zen
     (tab-bar-mode -1)
     (hide-mode-line-mode 1)
     (lsp-headerline-breadcrumb-mode -1))
    ('normal
     (tab-bar-mode 1)
     (hide-mode-line-mode -1)
     (lsp-headerline-breadcrumb-mode 1))
    (_ (user-error "Unknown UI mode: %S (expected 'zen or 'normal)" mode))))

(defun my-gui/hide-ui ()
  "Hides bars and any other distractions (zen mode)."
  (interactive)
  (my-gui/switch-ui 'zen))


(defun my-gui/hide-ui ()
  "Shows bars and any other distractions (zen mode)."
  (interactive)
  (my-gui/switch-ui 'normal))

(provide 'gui-settings)
;;; gui-settings.el ends here
