;;; temp-layout-settings.el --- Buffer routing to elastic frame -*- lexical-binding: t; -*-

;;; Commentary:
;; Routes buffers to tagged windows ("emux-slot") in the elastic child frame.
;; Uses `display-buffer-alist' to intercept buffer display and redirect
;; matching buffers (by mode or name) to specific slots/tabs in the elastic frame.

;;; Code:

(require 'cl-lib)

;; ============================================================================
;; Routing Rules
;; ============================================================================

(defvar my-layout--routing-rules nil
  "List of routing rule plists.
Each rule is (:modes MODES :regexp REGEXP :slot SLOT).
Exactly one of :modes or :regexp is non-nil.")

(cl-defun my-layout/make-route-buffer-rule (&key modes regexp slot)
  "Create a routing rule plist.
Exactly one of MODES (list of major mode symbols) or REGEXP (buffer name
pattern) must be provided. SLOT is the emux-slot tag string of the target
window."
  (unless slot
    (error "my-layout/make-route-buffer-rule: :slot is required"))
  (when (and modes regexp)
    (error "my-layout/make-route-buffer-rule: provide :modes or :regexp, not both"))
  (unless (or modes regexp)
    (error "my-layout/make-route-buffer-rule: one of :modes or :regexp is required"))
  (list :modes modes :regexp regexp :slot slot))

;; ============================================================================
;; Window Lookup
;; ============================================================================

(defun my-layout/find-window-by-slot-name (slot-name)
  "Find a live window with `emux-slot' parameter equal to SLOT-NAME.
Searches all frames including child frames.  Returns the window or nil."
  (let ((found nil))
    (walk-windows
     (lambda (w)
       (when (and (not found)
                  (equal (window-parameter w 'emux-slot) slot-name))
         (setq found w)))
     nil t)
    found))

;; ============================================================================
;; Elastic Frame Helpers
;; ============================================================================

(defun my-layout--elastic-frame ()
  "Return the live elastic vterm frame, or nil."
  (when (and (boundp 'elastic-vterm--frame)
             elastic-vterm--frame
             (frame-live-p elastic-vterm--frame))
    elastic-vterm--frame))

(defun my-layout--ensure-elastic-visible ()
  "Make the elastic frame visible if it exists but is hidden.
Returns the frame or nil."
  (when-let ((frame (my-layout--elastic-frame)))
    (unless (frame-visible-p frame)
      (make-frame-visible frame))
    frame))

(defun my-layout--find-or-create-tab (frame tab-name)
  "In FRAME, switch to tab named TAB-NAME or create it.
Returns the selected window in that tab."
  (let ((original-frame (selected-frame)))
    (unwind-protect
        (progn
          (select-frame frame)
          ;; Check if tab already exists
          (let* ((tabs (funcall tab-bar-tabs-function frame))
                 (existing (cl-find-if
                            (lambda (tab)
                              (equal (alist-get 'name tab) tab-name))
                            tabs)))
            (if existing
                (tab-bar-switch-to-tab tab-name)
              ;; Create new tab
              (tab-bar-new-tab)
              (tab-bar-rename-tab tab-name))
            (frame-selected-window frame)))
      (select-frame original-frame))))

(defun my-layout--create-slot-in-elastic (buffer slot-name)
  "Create a new tab in the elastic frame for SLOT-NAME and display BUFFER.
Returns the window or nil if elastic frame doesn't exist."
  (when-let ((frame (my-layout--ensure-elastic-visible)))
    (let ((original-frame (selected-frame))
          (tab-name (replace-regexp-in-string "^elastic-" "" slot-name)))
      (unwind-protect
          (let ((win (my-layout--find-or-create-tab frame tab-name)))
            (set-window-parameter win 'emux-slot slot-name)
            (set-window-buffer win buffer)
            win)
        ;; Restore focus to original frame
        (select-frame-set-input-focus original-frame)))))

;; ============================================================================
;; Rule Matching
;; ============================================================================

(defun my-layout--match-rule (buffer rule)
  "Return non-nil if BUFFER matches RULE."
  (let ((modes (plist-get rule :modes))
        (regexp (plist-get rule :regexp)))
    (cond
     (modes
      (with-current-buffer buffer
        (cl-some (lambda (mode) (derived-mode-p mode)) modes)))
     (regexp
      (string-match-p regexp (buffer-name buffer))))))

(defun my-layout--find-matching-rule (buffer)
  "Return the first routing rule matching BUFFER, or nil."
  (cl-find-if (lambda (rule) (my-layout--match-rule buffer rule))
              my-layout--routing-rules))

;; ============================================================================
;; Display Buffer Function
;; ============================================================================

(defun my-layout/route-buffer (buffer alist)
  "Display BUFFER according to routing rules.
For use in `display-buffer-alist'.  Checks BUFFER against all rules
in `my-layout--routing-rules'.  If a rule matches, routes the buffer
to the tagged window in the elastic frame.  Returns the window or nil."
  (when-let ((rule (my-layout--find-matching-rule buffer)))
    (let* ((slot-name (plist-get rule :slot))
           (existing-win (my-layout/find-window-by-slot-name slot-name)))
      (cond
       ;; Slot window exists: reuse it
       (existing-win
        (my-layout--ensure-elastic-visible)
        ;; Switch to the correct tab in the elastic frame
        (when-let ((frame (window-frame existing-win)))
          (let ((original-frame (selected-frame))
                (tab-name (replace-regexp-in-string "^elastic-" "" slot-name)))
            (unwind-protect
                (progn
                  (select-frame frame)
                  (tab-bar-switch-to-tab tab-name))
              (select-frame-set-input-focus original-frame))))
        (set-window-buffer existing-win buffer)
        existing-win)
       ;; No slot window yet: create tab in elastic frame
       ((my-layout--elastic-frame)
        (my-layout--create-slot-in-elastic buffer slot-name))
       ;; No elastic frame at all: fall through
       (t nil)))))

;; ============================================================================
;; Rules Registration
;; ============================================================================

(setq my-layout--routing-rules nil)

(push (my-layout/make-route-buffer-rule
       :modes '(help-mode)
       :slot "elastic-help")
      my-layout--routing-rules)

;; Install as first entry in display-buffer-alist (catch-all, returns nil for non-matches)
(add-to-list 'display-buffer-alist
             '("." (my-layout/route-buffer)))

(provide 'temp-layout-settings)
;;; temp-layout-settings.el ends here
