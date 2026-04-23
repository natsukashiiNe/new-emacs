;;; sidebar-settings.el --- Dirvish sidebar configuration. -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings for dirvish-side sidebar and custom attributes (vc-state-text).

;;; Code:

(require 'dirvish)
(require 'dirvish-vc)

;; == VC-STATE-TEXT ATTRIBUTE ==========================================

(defvar my/dirvish-vc-state-symbol-alist
  '((edited            . "")
    (added             . "")
    (removed           . "")
    (missing           . "")
    (conflict          . "")
    (unregistered      . "")
    (needs-merge       . "󰓡")
    (needs-update      . "󰁈")
    (unlocked-changes  . "")
    (up-to-date        . "")
    (ignored           . ""))
  "Alist mapping VC states to display symbols for `vc-state-text' attribute.
Each entry is (STATE . SYMBOL-STRING).")

(defvar my/dirvish-vc-state-face-alist
  '((edited            . diff-hl-change)
    (added             . diff-hl-insert)
    (removed           . diff-hl-delete)
    (missing           . diff-hl-delete)
    (conflict          . diff-hl-delete)
    (unregistered      . font-lock-comment-face)
    (needs-merge       . error)
    (needs-update      . warning)
    (unlocked-changes  . diff-hl-change)
    (up-to-date        . font-lock-comment-face)
    (ignored           . font-lock-comment-face))
  "Alist mapping VC states to faces for `vc-state-text' attribute.
Each entry is (STATE . FACE).  Uses `diff-hl' faces for consistency.")

(dirvish-define-attribute vc-state-text
  "Version control state as a colored text column."
  :when (and (symbolp (dirvish-prop :vc-backend))
             (not (dirvish-prop :remote)))
  :right 3
  (let* ((state (dirvish-attribute-cache f-name :vc-state))
         (sym   (or (alist-get state my/dirvish-vc-state-symbol-alist) " "))
         (face  (alist-get state my/dirvish-vc-state-face-alist))
         (str   (concat " " sym " ")))
    (when face
      (add-face-text-property 0 (length str) face t str))
    `(right . ,str)))

;; == SIDEBAR SETTINGS =================================================

(setq dirvish-side-width 37)
(setq dirvish-side-auto-expand t)
(setq dirvish-side-attributes '(vc-state-text nerd-icons))
(setq dirvish-side-header-line-format '(:left (project)))
(setq dirvish-side-mode-line-format nil)

(provide 'sidebar-settings)
;;; sidebar-settings.el ends here
