;;; lsp-signature-settings.el --- LSP signature popup -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom LSP signature popup with two states:
;;
;;   * posframe at frame top, when at least `:max-height' + 2 screen lines
;;     fit above point in the current window.
;;   * fallback to lsp-mode's default `lv' renderer at the echo area, when
;;     the top posframe would obstruct the area being typed in.
;;
;; The choice is re-evaluated on every scroll (mouse wheel, C-e, evil
;; scroll commands, page-down, ...).  The popup is hidden when the user
;; leaves the originating window or buffer.
;;
;; Toggle with `my-lsp/pop-up-signature-mode'.  When disabled, lsp-mode's
;; signature auto-activation is turned off and any active popup is hidden
;; -- restoring stock behavior with zero overhead.

;;; Code:

;; All renderer dependencies (posframe / lv / lsp-mode) load lazily through
;; their respective use-package blocks; this file does not `require' them at
;; top level because Elpaca may not have installed them yet by the time
;; `lsp-setup' loads.  The functions are only called via
;; `lsp-signature-function', which fires after the LSP session is up -- by
;; then everything below is bound.
(declare-function posframe-show                         "posframe")
(declare-function posframe-hide                         "posframe")
(declare-function posframe-poshandler-frame-top-center  "posframe")
(declare-function lv-delete-window                      "lv")
(declare-function lsp-lv-message                        "lsp-mode")

(defvar lsp-signature-function)
(defvar lsp-signature-auto-activate)
(defvar lsp-signature-render-documentation)

;; =============================================================================
;; PARAMETERS
;; =============================================================================

(defvar my-lsp--signature-posframe-params
  (list :poshandler #'posframe-poshandler-frame-top-center
        :min-height 1
        :max-height 13
        :min-width  60
        :max-width  80
        :border-width 1
        :border-color "gray50"
        :internal-border-width 6)
  "Posframe params used by the top-of-frame signature popup.")

;; =============================================================================
;; STATE
;; =============================================================================

(defvar my-lsp--signature-state nil
  "Plist describing the live popup, or nil when nothing is shown.
Keys: :str STR :window W :buffer B :renderer (posframe|lv).")

(defvar my-lsp--signature-saved nil
  "Snapshot of overridden `lsp-signature-*' vars, restored on mode disable.")

;; =============================================================================
;; FIT DECISION
;; =============================================================================

(defun my-lsp--signature-fits-on-top-p ()
  "Return non-nil when the cursor sits far enough below the FRAME top for
the popup to fit above it with two lines of headroom.

The popup is anchored to the frame top, so the fit is computed against
the frame -- not just the current window.  In a vertical split with
point in the bottom window, there is still room above as long as the
top of the cursor's line sits below `:max-height' + 2 lines from the
frame top.

Conservative on height: assumes the popup will render at full
`:max-height'.  Returns nil if point's screen position is unknown
(e.g. point scrolled off the window)."
  (let ((posn (posn-at-point)))
    (when posn
      (let* ((y-px           (cdr (posn-x-y posn)))
             (window-body-top (nth 1 (window-inside-pixel-edges)))
             (char-h         (frame-char-height))
             (cursor-y       (+ window-body-top y-px))
             (needed-px      (* char-h
                                (+ (plist-get my-lsp--signature-posframe-params
                                              :max-height)
                                   2))))
        (>= cursor-y needed-px)))))

;; =============================================================================
;; RENDERERS
;; =============================================================================

(defun my-lsp--signature-render-posframe-top (str)
  "Render STR via posframe at the frame top."
  (when (fboundp 'lv-delete-window) (lv-delete-window))
  (apply #'posframe-show
         (with-current-buffer (get-buffer-create " *lsp-signature*")
           (erase-buffer)
           (insert str)
           (visual-line-mode 1)
           (current-buffer))
         (append
          my-lsp--signature-posframe-params
          (list :position (point)
                :background-color (face-attribute 'lsp-signature-posframe
                                                  :background nil t)
                :foreground-color (face-attribute 'lsp-signature-posframe
                                                  :foreground nil t)))))

(defun my-lsp--signature-render-lv-fallback (str)
  "Render STR via lsp-mode's default `lv' renderer."
  (posframe-hide " *lsp-signature*")
  (lsp-lv-message str))

;; =============================================================================
;; PUBLIC RENDERER -- used as `lsp-signature-function'
;; =============================================================================

(defun my-lsp/signature-show (str)
  "Custom `lsp-signature-function'.
Render STR on top via posframe when it fits, otherwise via lv.  STR nil
hides both renderers."
  (if (null str)
      (progn
        (posframe-hide " *lsp-signature*")
        (when (fboundp 'lv-delete-window) (lv-delete-window))
        (setq my-lsp--signature-state nil))
    (let ((renderer (if (my-lsp--signature-fits-on-top-p) 'posframe 'lv)))
      (setq my-lsp--signature-state
            (list :str str
                  :window (selected-window)
                  :buffer (current-buffer)
                  :renderer renderer))
      (if (eq renderer 'posframe)
          (my-lsp--signature-render-posframe-top str)
        (my-lsp--signature-render-lv-fallback str)))))

;; =============================================================================
;; AUTO-RECOMPUTE ON SCROLL / WINDOW / BUFFER CHANGES
;; =============================================================================

(defun my-lsp--signature-restate ()
  "Re-run the renderer for the cached signature string.
Used to swap posframe<->lv as the cursor scrolls in or out of room."
  (let ((state my-lsp--signature-state))
    (when (and state
               (buffer-live-p (plist-get state :buffer))
               (window-live-p (plist-get state :window)))
      (with-selected-window (plist-get state :window)
        (with-current-buffer (plist-get state :buffer)
          (my-lsp/signature-show (plist-get state :str)))))))

(defun my-lsp--signature-on-scroll (window _new-start)
  "Defer a re-render when WINDOW holds the active popup.
Runs at idle to escape the redisplay context of `window-scroll-functions'."
  (when (and (bound-and-true-p my-lsp/pop-up-signature-mode)
             my-lsp--signature-state
             (eq window (plist-get my-lsp--signature-state :window)))
    (run-at-time 0 nil #'my-lsp--signature-restate)))

(defun my-lsp--signature-on-window-change (&optional _frame)
  "Hide the popup when point leaves the originating window or buffer."
  (when (and my-lsp--signature-state
             (or (not (eq (selected-window)
                          (plist-get my-lsp--signature-state :window)))
                 (not (eq (current-buffer)
                          (plist-get my-lsp--signature-state :buffer)))))
    (posframe-hide " *lsp-signature*")
    (when (fboundp 'lv-delete-window) (lv-delete-window))
    (setq my-lsp--signature-state nil)))

;; =============================================================================
;; MINOR MODE
;; =============================================================================

;;;###autoload
(define-minor-mode my-lsp/pop-up-signature-mode
  "Toggle the LSP signature popup.

When ON, the signature renders via posframe at the frame top if it fits
with two lines of headroom above point, otherwise via `lv' at the echo
area.  Placement is re-evaluated on scroll and the popup is hidden when
the user leaves the originating window or buffer.

When OFF, lsp-mode's signature auto-activation is disabled, any active
popup is hidden, and the previously-configured `lsp-signature-function'
is restored -- behavior matches stock lsp-mode."
  :global t
  :group 'lsp-mode
  (require 'lsp-mode)
  (if my-lsp/pop-up-signature-mode
      (progn
        (unless my-lsp--signature-saved
          (setq my-lsp--signature-saved
                (list :fn   lsp-signature-function
                      :auto lsp-signature-auto-activate)))
        (setq lsp-signature-function      #'my-lsp/signature-show
              lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
        (add-hook 'window-scroll-functions           #'my-lsp--signature-on-scroll)
        (add-hook 'window-buffer-change-functions    #'my-lsp--signature-on-window-change)
        (add-hook 'window-selection-change-functions #'my-lsp--signature-on-window-change))
    (remove-hook 'window-scroll-functions           #'my-lsp--signature-on-scroll)
    (remove-hook 'window-buffer-change-functions    #'my-lsp--signature-on-window-change)
    (remove-hook 'window-selection-change-functions #'my-lsp--signature-on-window-change)
    (posframe-hide " *lsp-signature*")
    (when (fboundp 'lv-delete-window) (lv-delete-window))
    (setq my-lsp--signature-state nil)
    (when my-lsp--signature-saved
      (setq lsp-signature-function      (plist-get my-lsp--signature-saved :fn)
            lsp-signature-auto-activate (plist-get my-lsp--signature-saved :auto))
      (setq my-lsp--signature-saved nil))))

;; =============================================================================
;; ENABLE BY DEFAULT + SHARED SIGNATURE TUNABLES
;; =============================================================================

(with-eval-after-load 'lsp-mode
  (setq lsp-signature-render-documentation t)
  (my-lsp/pop-up-signature-mode 1))

(provide 'lsp-signature-settings)
;;; lsp-signature-settings.el ends here
