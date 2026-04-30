;;; flymake-diagnostics.el --- My inline flymake hints. -*- lexical-binding: t; -*-

;;; Commentary:
;; Diagnostic display for flymake with Evil state awareness:
;; - Normal mode: built-in EOL short diagnostics
;; - Insert mode: EOL disabled, quick-peek inline overlay under cursor

;;; Code:

;; -- End-of-line diagnostics --------------------------------------------------
(setq flymake-show-diagnostics-at-end-of-line 'short)
(setq flymake-no-changes-timeout 2)

;; =============================================================================
;; INLINE DIAGNOSTICS - quick-peek display on cursor hover
;; =============================================================================

(use-package quick-peek
  :ensure t
  :config
  ;; Configure display settings
  (setq quick-peek-add-spacer nil)
  
  ;; Fix face to properly extend background color
  (set-face-attribute 'quick-peek-background-face nil
		      :background nil
		      :inherit 'default
		      :stipple nil
		      :extend t))

(defvar my-flymake-qp-match 'line
  "How to match diagnostics for quick-peek display.
`exact' — only diagnostics exactly at point.
`line'  — all diagnostics on the current line.")

(defvar-local my-flymake--qp-timer nil
  "Idle timer for showing quick-peek flymake diagnostics.")

(defvar-local my-flymake--qp-showing nil
  "Non-nil when quick-peek overlay is currently displayed.")

(defvar-local my-flymake--qp-last-line nil
  "Last line number where diagnostics were shown.")

(defun my-flymake--qp-hide ()
  "Hide quick-peek flymake overlays."
  (when my-flymake--qp-showing
    (quick-peek-hide)
    (setq my-flymake--qp-showing nil)
    (setq my-flymake--qp-last-line nil)))

(defun my-flymake--qp-diagnostics ()
  "Collect diagnostics according to `my-flymake-qp-match'."
  (pcase my-flymake-qp-match
    ('exact (flymake-diagnostics (point)))
    ('line (flymake-diagnostics (line-beginning-position)
                                (line-end-position)))))

(defun my-flymake--qp-format (diagnostic)
  "Format a single flymake DIAGNOSTIC for quick-peek display."
  (let* ((type (flymake-diagnostic-type diagnostic))
         (face (flymake--lookup-type-property type 'mode-line-face))
         (label (flymake--lookup-type-property type 'flymake-type-name))
         (msg (flymake-diagnostic-text diagnostic)))
    (concat (propertize (upcase label) 'face face) ": " msg)))

(defun my-flymake--qp-show ()
  "Show flymake diagnostics for the current line via quick-peek."
  (when (and (fboundp 'quick-peek-show)
             (my-flymake--qp-diagnostics))
    (let ((text (mapconcat #'my-flymake--qp-format
                           (my-flymake--qp-diagnostics) "\n")))
      (quick-peek-show text (line-beginning-position))
      (setq my-flymake--qp-showing t)
      (setq my-flymake--qp-last-line (line-number-at-pos)))))

(defun my-flymake--qp-refresh ()
  "Refresh display when diagnostics are updated by flymake."
  (when (and my-flymake--qp-showing
             (= (line-number-at-pos) (or my-flymake--qp-last-line 0)))
    (my-flymake--qp-hide)
    (my-flymake--qp-show)))

(defun my-flymake--qp-post-command ()
  "Handle cursor movement: hide/show only on line change."
  (let ((cur-line (line-number-at-pos)))
    (cond
     ;; Same line — do nothing, keep overlay as-is
     ((and my-flymake--qp-last-line (= cur-line my-flymake--qp-last-line))
      nil)
     ;; Line changed — hide old overlay, schedule new one
     (t
      (my-flymake--qp-hide)
      (when my-flymake--qp-timer
        (cancel-timer my-flymake--qp-timer))
      (setq my-flymake--qp-timer
            (run-with-idle-timer 0.3 nil #'my-flymake--qp-show))))))

(define-minor-mode my-flymake-quick-peek-mode
  "Show flymake diagnostics at point via quick-peek overlays.
Set `my-flymake-qp-match' to `exact' or `line' to control scope."
  :lighter nil
  (if my-flymake-quick-peek-mode
      (progn
        (add-hook 'post-command-hook #'my-flymake--qp-post-command nil t)
        (add-hook 'flymake-after-diagnostics-hook
                  #'my-flymake--qp-refresh nil t))
    (my-flymake--qp-hide)
    (when my-flymake--qp-timer
      (cancel-timer my-flymake--qp-timer)
      (setq my-flymake--qp-timer nil))
    (remove-hook 'post-command-hook #'my-flymake--qp-post-command t)
    (remove-hook 'flymake-after-diagnostics-hook
                 #'my-flymake--qp-refresh t)))

;; =============================================================================
;; EVIL STATE INTEGRATION
;; =============================================================================
;; Normal: EOL 'short ON, quick-peek OFF
;; Insert: EOL OFF, quick-peek ON (inline under cursor)

(defun my-flymake--hide-eol-overlays ()
  "Hide all flymake EOL overlays by clearing their display property."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'flymake--eol-overlay)
      (overlay-put ov 'display nil))))

(defun my-flymake--redraw-eol-overlays ()
  "Regenerate display for all flymake EOL overlays from current diagnostics."
  (flymake--update-eol-overlays))

;; TODO: quick-peek inline removed for now, needs further testing.
;; Proved to be unstable.

(defun my-flymake--enter-insert-state ()
  "Switch to insert-mode diagnostics: disable EOL, enable quick-peek."
  (when (bound-and-true-p flymake-mode)
    (setq-local flymake-show-diagnostics-at-end-of-line nil)
    (my-flymake--hide-eol-overlays)
    ;; (my-flymake-quick-peek-mode 1)
    ))

(defun my-flymake--exit-insert-state ()
  "Switch to 'normal-mode' diagnostics: enable EOL, disable quick-peek."
  (when (bound-and-true-p flymake-mode)
    ;; (my-flymake-quick-peek-mode -1)
    (setq-local flymake-show-diagnostics-at-end-of-line 'short)
    (my-flymake--redraw-eol-overlays)))

(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'my-flymake--enter-insert-state)
  (add-hook 'evil-insert-state-exit-hook #'my-flymake--exit-insert-state))

(defun my/evil-toggle-enter-insert-flymake-hook ()
  "Toggle flymake quick-peek activation on evil insert entry."
  (interactive)
  (if (memq #'my-flymake--enter-insert-state evil-insert-state-entry-hook)
      (progn
        (remove-hook 'evil-insert-state-entry-hook #'my-flymake--enter-insert-state)
        (message "Flymake insert-entry hook disabled"))
    (add-hook 'evil-insert-state-entry-hook #'my-flymake--enter-insert-state)
    (message "Flymake insert-entry hook enabled")))

(defun my/evil-toggle-exit-insert-flymake-hook ()
  "Toggle flymake EOL restore on evil insert exit."
  (interactive)
  (if (memq #'my-flymake--exit-insert-state evil-insert-state-exit-hook)
      (progn
        (remove-hook 'evil-insert-state-exit-hook #'my-flymake--exit-insert-state)
        (message "Flymake insert-exit hook disabled"))
    (add-hook 'evil-insert-state-exit-hook #'my-flymake--exit-insert-state)
    (message "Flymake insert-exit hook enabled")))

(provide 'flymake-diagnostics)
;;; flymake-diagnostics.el ends here
