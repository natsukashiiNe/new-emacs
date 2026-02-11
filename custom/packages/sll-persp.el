;;; sll-persp.el --- Perspective integration for SLL -*- lexical-binding: t; -*-

;; Author: natsukashii
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (slot-layout "2.0.0"))
;; Keywords: convenience, frames, windows

;;; Commentary:
;; Makes SLL layout state perspective-local.
;; Each perspective maintains its own layout, sublayout, and slot visibility.
;;
;; When switching perspectives:
;; - Current SLL state is saved
;; - Target perspective's SLL state is restored (if any)
;; - If no saved state, layout is unloaded
;;
;; Usage:
;;   (require 'sll-persp)
;;   (sll-persp-mode 1)
;;
;; Or with use-package:
;;   (use-package sll-persp
;;     :after (slot-layout perspective)
;;     :config (sll-persp-mode 1))

;;; Code:

(require 'slot-layout)

;;; ============================================================================
;;; Custom Variables
;;; ============================================================================

(defgroup sll-persp nil
  "Perspective integration for SLL."
  :group 'slot-layout
  :prefix "sll-persp-")

(defcustom sll-persp-verbose nil
  "When non-nil, show messages about perspective state changes."
  :type 'boolean
  :group 'sll-persp)

;;; ============================================================================
;;; Internal State
;;; ============================================================================

(defvar sll-persp--state (make-hash-table :test 'equal)
  "Hash table: (frame-id . persp-name) -> state-plist.
State includes layout-name, sublayout-name, hidden-slots, slot-buffers.")

(defvar sll-persp--frame-counter 0
  "Counter for generating unique frame IDs.")

;;; ============================================================================
;;; Frame Identification
;;; ============================================================================

(defun sll-persp--get-frame-id (&optional frame)
  "Get or create unique ID for FRAME."
  (let ((f (or frame (selected-frame))))
    (or (frame-parameter f 'sll-persp-frame-id)
        (let ((id (format "frame-%d-%d" (emacs-pid) (cl-incf sll-persp--frame-counter))))
          (set-frame-parameter f 'sll-persp-frame-id id)
          id))))

(defun sll-persp--make-key (&optional frame)
  "Create a key for the current FRAME + perspective."
  (cons (sll-persp--get-frame-id frame)
        (if (bound-and-true-p persp-mode)
            (safe-persp-name (get-current-persp))
          "default")))

;;; ============================================================================
;;; State Capture
;;; ============================================================================

(defun sll-persp--capture-slot-buffers ()
  "Capture current buffer name in each visible slot."
  (let ((buffers nil))
    (maphash
     (lambda (slot window)
       (when (window-live-p window)
         (let ((buf (window-buffer window)))
           (when (buffer-live-p buf)
             (push (cons slot (buffer-name buf)) buffers)))))
     sll--slot-windows)
    buffers))

(defun sll-persp--capture-state ()
  "Capture current SLL state as a plist.
Returns nil if no layout is active."
  (when sll-current-layout-name
    (list :layout-name sll-current-layout-name
          :sublayout-name sll-current-sublayout-name
          :hidden-slots (copy-sequence sll-current-hidden-slots)
          :slot-buffers (sll-persp--capture-slot-buffers))))

;;; ============================================================================
;;; State Restoration
;;; ============================================================================

(defun sll-persp--restore-slot-buffers (slot-buffers)
  "Restore buffers to slots from SLOT-BUFFERS alist."
  (dolist (pair slot-buffers)
    (let* ((slot (car pair))
           (buf-name (cdr pair))
           (buf (get-buffer buf-name))
           (win (sll--get-slot-window slot)))
      (when (and buf (buffer-live-p buf) win (window-live-p win))
        (set-window-buffer win buf)))))

(defun sll-persp--restore-state (state)
  "Restore SLL state from STATE plist.
If STATE is nil, unloads any active layout."
  (if state
      (let ((layout-name (plist-get state :layout-name)))
        (if (and layout-name (gethash layout-name sll--layouts))
            (progn
              ;; Load the layout (this sets up windows)
              (sll-load-layout layout-name)
              ;; Restore sublayout if different from full
              (let ((sublayout (plist-get state :sublayout-name)))
                (when (and sublayout
                           (not (eq sublayout 'full))
                           (memq sublayout (sll-available-sublayouts)))
                  (ignore-errors (sll-switch-sublayout sublayout))))
              ;; Restore additional hidden slots not covered by sublayout
              (let ((hidden (plist-get state :hidden-slots)))
                (dolist (slot hidden)
                  (when (and (memq slot (sll-available-slots))
                             (not (eq slot 'MAIN))
                             (not (memq slot sll-current-hidden-slots)))
                    (ignore-errors (sll-hide-slot slot)))))
              ;; Try to restore buffers to their slots
              (let ((slot-buffers (plist-get state :slot-buffers)))
                (when slot-buffers
                  (sll-persp--restore-slot-buffers slot-buffers)))
              (when sll-persp-verbose
                (message "SLL-Persp: Restored layout '%s'" layout-name)))
          ;; Layout not defined - clear state
          (when sll-current-layout-name
            (sll-unload-layout))
          (when sll-persp-verbose
            (message "SLL-Persp: Layout '%s' not found, unloaded" layout-name))))
    ;; No state - unload any active layout
    (when sll-current-layout-name
      (sll-unload-layout)
      (when sll-persp-verbose
        (message "SLL-Persp: No saved state, layout unloaded")))))

;;; ============================================================================
;;; Perspective Hooks
;;; ============================================================================

(defun sll-persp--save-current-state ()
  "Save current SLL state for the current perspective."
  (let ((key (sll-persp--make-key))
        (state (sll-persp--capture-state)))
    (if state
        (puthash key state sll-persp--state)
      ;; No layout active - remove any saved state
      (remhash key sll-persp--state))
    (when sll-persp-verbose
      (message "SLL-Persp: Saved state for %s" key))))

(defun sll-persp--on-switch-before ()
  "Hook run before switching perspectives.
Saves the current SLL state."
  (sll-persp--save-current-state))

(defun sll-persp--on-switch-after ()
  "Hook run after switching perspectives.
Restores the SLL state for the new perspective."
  ;; Small delay to let perspective finish restoring windows
  (run-with-timer
   0.05 nil
   (lambda ()
     (let* ((key (sll-persp--make-key))
            (state (gethash key sll-persp--state)))
       (sll-persp--restore-state state)))))

(defun sll-persp--on-persp-created ()
  "Hook run when a new perspective is created.
New perspectives start without a layout."
  (when sll-persp-verbose
    (message "SLL-Persp: New perspective created, no layout")))

(defun sll-persp--on-persp-killed ()
  "Hook run when a perspective is killed.
Removes saved state for the killed perspective."
  (let ((key (sll-persp--make-key)))
    (remhash key sll-persp--state)
    (when sll-persp-verbose
      (message "SLL-Persp: Removed state for killed perspective"))))

(defun sll-persp--on-frame-created (frame)
  "Initialize FRAME with unique ID."
  (sll-persp--get-frame-id frame))

;;; ============================================================================
;;; Minor Mode
;;; ============================================================================

(defun sll-persp--enable ()
  "Enable SLL perspective integration."
  ;; Initialize current frame
  (sll-persp--get-frame-id)
  ;; Initialize new frames
  (add-hook 'after-make-frame-functions #'sll-persp--on-frame-created)
  ;; Hook into perspective
  (when (bound-and-true-p persp-mode)
    (add-hook 'persp-before-switch-hook #'sll-persp--on-switch-before)
    (add-hook 'persp-activated-hook #'sll-persp--on-switch-after)
    (add-hook 'persp-created-hook #'sll-persp--on-persp-created)
    (add-hook 'persp-killed-hook #'sll-persp--on-persp-killed))
  (message "SLL perspective integration enabled"))

(defun sll-persp--disable ()
  "Disable SLL perspective integration."
  (remove-hook 'after-make-frame-functions #'sll-persp--on-frame-created)
  (remove-hook 'persp-before-switch-hook #'sll-persp--on-switch-before)
  (remove-hook 'persp-activated-hook #'sll-persp--on-switch-after)
  (remove-hook 'persp-created-hook #'sll-persp--on-persp-created)
  (remove-hook 'persp-killed-hook #'sll-persp--on-persp-killed)
  (message "SLL perspective integration disabled"))

;;;###autoload
(define-minor-mode sll-persp-mode
  "Minor mode for perspective-local SLL layouts.
When enabled, each perspective maintains its own SLL layout state."
  :global t
  :lighter " SLL-P"
  :group 'sll-persp
  (if sll-persp-mode
      (sll-persp--enable)
    (sll-persp--disable)))

;;; ============================================================================
;;; Utility Commands
;;; ============================================================================

;;;###autoload
(defun sll-persp-clear-state ()
  "Clear all saved SLL perspective state."
  (interactive)
  (clrhash sll-persp--state)
  (message "SLL-Persp: All saved state cleared"))

;;;###autoload
(defun sll-persp-describe-state ()
  "Show current SLL perspective state."
  (interactive)
  (let ((key (sll-persp--make-key))
        (state (sll-persp--capture-state)))
    (with-help-window "*SLL Perspective State*"
      (princ (format "Current Key: %s\n\n" key))
      (princ "Current State:\n")
      (if state
          (progn
            (princ (format "  Layout: %s\n" (plist-get state :layout-name)))
            (princ (format "  Sublayout: %s\n" (plist-get state :sublayout-name)))
            (princ (format "  Hidden: %s\n" (plist-get state :hidden-slots)))
            (princ (format "  Slot Buffers: %s\n" (plist-get state :slot-buffers))))
        (princ "  No layout active\n"))
      (princ "\nSaved States:\n")
      (if (= (hash-table-count sll-persp--state) 0)
          (princ "  None\n")
        (maphash
         (lambda (k v)
           (princ (format "  %s -> %s\n" k (plist-get v :layout-name))))
         sll-persp--state)))))

(provide 'sll-persp)
;;; sll-persp.el ends here
