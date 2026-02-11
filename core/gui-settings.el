;;; gui-settings.el --- Settings for GUI/TTY frames. -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains settings of interface.
;; Settings in this files would be applied to newly created frames.
;; It can depend on whether Emacs is running in GUI or TTY modes.
;; Describes themes, frame parameters, bar settings, dynamically changing faces.


;;; Code:

;; == FRAME SETTINGS ======================================================

(load-theme 'my-modus-mono-dark t)

(defun reload-my-theme()
  (interactive)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" my-config-dir))
  (load-theme 'my-modus-mono-dark t))
;; TODO: move to INTERFACE theme (C-c u) + toggling of terminal, divish, flycheck, resizing etc.
(keymap-set global-map "C-c R" #'reload-my-theme)

(defun my/setup-tty-frame (&optional frame)
  "Apply settings for TTY frames."
  (with-selected-frame (or frame (selected-frame))
    (when (not (display-graphic-p frame))
      (set-face-background 'default "unspecified" ))))

(defun my/setup-gui-frame (&optional frame)
  "Apply settings for GUI frames."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p frame)
      (set-face-attribute 'default frame :font "GoMono Nerd Font-21")
      (set-face-attribute 'variable-pitch frame :font "GoMono Nerd Font-20")
      (set-frame-parameter frame 'alpha-background 92))))

(defun my/setup-frame (&optional frame)
  "Apply appropriate settings based on frame type."
  (if (display-graphic-p frame)
      (my/setup-gui-frame frame)
    (my/setup-tty-frame frame)))

(my/setup-frame)
(add-hook 'after-make-frame-functions 'my/setup-frame)


;; == TAB BAR ========================================================

;; == MODE LINE ======================================================

(provide 'gui-settings)
;;; gui-settings.el ends here
