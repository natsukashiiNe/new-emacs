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



(defun reload-my-theme()
  (interactive)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" my-config-dir))
  (load-theme (car custom-enabled-themes ) t))
;; TODO: move to INTERFACE theme (C-c u) + toggling of terminal, divish, flycheck, resizing etc.
(keymap-set global-map "C-c R" #'reload-my-theme)

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

(provide 'gui-settings)
;;; gui-settings.el ends here
