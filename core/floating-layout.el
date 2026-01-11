;;; floating-layout.el --- Define child-frames for floating windows. -*- lexical-binding: t; -*-

;;; Commentary:
;; Setting to enable floating windows.

;;; Code:
(defvar my/use-popup-next nil
  "When non-nil, next buffer opens in popup frame.")

(defun my/popup-frame-show (buffer)
  "Show BUFFER in centered child frame with visible borders."
  ;; Delete existing popup if any
  (posframe-delete-all)

  ;; Show new popup with explicit border settings
  (posframe-show buffer
                 :poshandler 'posframe-poshandler-frame-center
                 :width (round (* 0.75 (frame-width)))
                 :height (round (* 0.60 (frame-height)))
                 :border-width 3
                 :border-color "#61AFEF"
                 :internal-border-width 15
                 :internal-border-color "#282c34"
                 :background-color nil  ; Use buffer's background
                 :accept-focus t
                 :respect-mode-line t
                 :override-parameters '((left-fringe . 8)
                                        (right-fringe . 8)
                                        (undecorated . nil)  ; Try with decoration
                                        (no-accept-focus . nil)
                                        (no-focus-on-map . nil)
                                        (min-width . 0)
                                        (min-height . 0)
                                        (border-width . 3)
                                        (internal-border-width . 15)
                                        (vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)
                                        (left-fringe . 8)
                                        (right-fringe . 8)
                                        (menu-bar-lines . 0)
                                        (tool-bar-lines . 0)
                                        (tab-bar-lines . 0)
                                        (line-spacing . 0)
                                        (unsplittable . t)
                                        (no-other-frame . t)
                                        (no-special-glyphs . t)
                                        (desktop-dont-save . t)))
  (setq my/popup-frame-name buffer)

  ;; Ensure the child frame has focus
  (let ((frame (posframe--find-existing-posframe buffer)))
    (when frame
      (select-frame-set-input-focus frame))))

(defun my/popup-display-function (buffer alist)
  "Display function that shows BUFFER in popup if flagged."
  (when my/use-popup-next
    (setq my/use-popup-next nil)
    (my/popup-frame-show buffer)
    ;; Return the window (required by display-buffer protocol)
    (get-buffer-window buffer t)))

(defun my/popup-prefix ()
  "Prefix command: next buffer opens in popup frame."
  (interactive)
  (setq my/use-popup-next t)
  (message "Next buffer will open in popup frame"))

(defun my/popup-hide ()
  "Hide the popup frame."
  (interactive)
  (posframe-delete-all))

;; Add to display-buffer-alist as first entry (highest priority)
(add-to-list 'display-buffer-alist
             '(".*" (my/popup-display-function)))

;; Keybindings
(global-set-key (kbd "C-c w p") #'my/popup-prefix)
(global-set-key (kbd "C-c w q") #'my/popup-hide)

(provide 'floating-layout)
;;; floating-layout.el ends here
