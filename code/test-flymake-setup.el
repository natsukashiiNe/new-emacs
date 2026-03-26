;;; test-flymake-setup.el --- Standalone flymake test config -*- lexical-binding: t; -*-

;;; Commentary:
;; Test file to evaluate flymake as a sole diagnostic backend (no flycheck).
;; Load manually: M-x load-file RET ~/.config/emacs/code/test-flymake-setup.el
;; Or: (load-config-file "code/test-flymake-setup.el")
;;
;; What this does:
;; - Switches lsp-mode diagnostics provider to :flymake
;; - Enables flymake in prog-mode buffers
;; - Configures left-fringe indicators with custom bitmap
;; - Enables end-of-line diagnostic display
;; - Removes flymake-proc legacy backend
;; - Sets up consult-flymake if available
;;
;; After loading, restart LSP in your buffer: M-x lsp-workspace-restart

;;; Code:

;; =============================================================================
;; LSP INTEGRATION - Switch to flymake
;; =============================================================================

(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flymake))

;; =============================================================================
;; FLYMAKE - Core Setup
;; =============================================================================

(require 'flymake)

;; -- Fringe indicators --------------------------------------------------------
(define-fringe-bitmap 'my-flymake-fringe-indicator
  (vector #b0000011111100000)
  nil 16 '(top t))

(setq flymake-indicator-type 'fringes)
(setq flymake-fringe-indicator-position 'left-fringe)

;; Apply custom bitmap to all severity levels
(setq flymake-error-bitmap   '(my-flymake-fringe-indicator flymake-error-fringe))
(setq flymake-warning-bitmap '(my-flymake-fringe-indicator flymake-warning-fringe))
(setq flymake-note-bitmap    '(my-flymake-fringe-indicator flymake-note-fringe))

;; -- End-of-line diagnostics --------------------------------------------------
(setq flymake-show-diagnostics-at-end-of-line t)

;; -- Suppress flymake-proc legacy backend -------------------------------------
(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; -- Activate in prog-mode ----------------------------------------------------
(add-hook 'prog-mode-hook #'flymake-mode)

;; =============================================================================
;; CONSULT-FLYMAKE (if available)
;; =============================================================================

(with-eval-after-load 'consult
  (when (locate-library "consult-flymake")
    (autoload 'consult-flymake "consult-flymake" nil t)))

;; =============================================================================
;; INLINE DIAGNOSTICS - quick-peek display on cursor hover
;; =============================================================================

(require 'quick-peek nil t)

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
  (when-let* ((diagnostics (my-flymake--qp-diagnostics)))
    (let* ((pos (line-beginning-position))
           (ov (quick-peek-overlay-ensure-at pos))
           (text (mapconcat #'my-flymake--qp-format diagnostics "\n")))
      (setf (quick-peek-overlay-contents ov) text)
      (quick-peek-update ov)
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

(add-hook 'flymake-mode-hook #'my-flymake-quick-peek-mode)

(provide 'test-flymake-setup)
;;; test-flymake-setup.el ends here
