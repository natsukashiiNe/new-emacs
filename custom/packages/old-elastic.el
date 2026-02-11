;;; elastic.el --- Child-frames control -*- lexical-binding: t; -*-

;;; Commentary:
;; Package to control child frames and theirs content.

;;; Code:

(defvar elastic-vterm--frame nil
  "The floating vterm frame used by elastic-vterm.
This frame is reused when toggling the terminal.")

(defvar floating-frame--parent (selected-frame)
  "The parent frame whose geometry is used as reference for floating frames.")

(defvar floating-frame--parent-geometry
  (list :width (frame-pixel-width floating-frame--parent)
        :height (frame-pixel-height floating-frame--parent))
  "A plist storing the current pixel geometry of `floating-frame--parent'.
The keys are :width and :height.")

(defun floating-frame--update-geometry (frame)
  "Update the stored geometry for FRAME if it is the parent frame.
This function is meant to be used in `after-change-frame-geometry-hook'."
  (when (eq frame floating-frame--parent)
    (setq floating-frame--parent-geometry
          (list :width (frame-pixel-width frame)
                :height (frame-pixel-height frame)))))

(add-hook 'after-change-frame-geometry-hook #'floating-frame--update-geometry)

;;;###autoload
(defun elastic-vterm ()
  "Launch a vterm in a new floating child frame.
The new frame's dimensions are computed as 70% of the parent frame's size,
with a 15% offset from the parent's left and top edges.
The vterm buffer has its mode-line and line numbers disabled.
This function creates and stores the frame in `elastic-vterm--frame`."
  (interactive)
  (let* ((parent floating-frame--parent)
         (geometry floating-frame--parent-geometry)
         (parent-width (plist-get geometry :width))
         (parent-height (plist-get geometry :height))
         ;; Calculate the desired child frame dimensions in pixels.
         (child-width-pixels (* 0.9 parent-width))
         (child-height-pixels (* 0.7 parent-height))
         (child-left (round (* 0.05 parent-width)))
         (child-top (round (* 0.15 parent-height)))
         ;; Convert pixel dimensions to text dimensions.
         (char-width (frame-char-width parent))
         (char-height (frame-char-height parent))
         (child-width-chars (max 1 (floor (/ child-width-pixels char-width))))
         (child-height-lines (max 1 (floor (/ child-height-pixels char-height))))
         ;; Build the frame parameters.
         (frame-params `((parent-frame . ,parent)
                         (title . "elastic-vterm")
                         (left . ,child-left)
                         (top . ,child-top)
                         (width . ,child-width-chars)
                         (height . ,child-height-lines)
                         (child-frame-border-width . 10)
                         (visibility . t)
                         (no-accept-focus . nil)
                         (undecorated . nil)
                         (keep-ratio . t))))
    (setq elastic-vterm--frame (make-frame frame-params))
    (select-frame-set-input-focus elastic-vterm--frame)
    ;; Launch vterm in the new frame if available.
    (when (fboundp 'vterm)
      (let ((vterm-buffer (vterm)))
        (set-window-buffer (frame-root-window elastic-vterm--frame) vterm-buffer)
        ;; Disable the mode-line and line numbers in the vterm buffer.
        (with-current-buffer vterm-buffer
          (setq-local mode-line-format nil)
          (when (boundp 'display-line-numbers)
            (setq-local display-line-numbers nil))
          (when (fboundp 'display-line-numbers-mode)
            (display-line-numbers-mode -1))
          ;; Disable the exit confirmation by clearing the process query flag.
          (when-let ((proc (get-buffer-process (current-buffer))))
            (set-process-query-on-exit-flag proc nil)))))))

;;;###autoload
(defun elastic-vterm-toggle ()
  "Toggle the visibility of the elastic vterm frame.
If the frame exists and is live, hide it if visible or show it if hidden.
If it doesn't exist, create one using `elastic-vterm`."
  (interactive)
  (if (and elastic-vterm--frame (frame-live-p elastic-vterm--frame))
      (if (frame-visible-p elastic-vterm--frame)
          (make-frame-invisible elastic-vterm--frame)
        (make-frame-visible elastic-vterm--frame)
	(other-frame 1)
	)
    (elastic-vterm)))

(provide 'elastic)
;;; elastic.el ends here
