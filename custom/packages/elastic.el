;;; elastic.el --- Child-frames control -*- lexical-binding: t; -*-

;;; Commentary:
;; Package to control elastic child frames for vterm.
;; Supports both global frame and perspective-local frames.
;;
;; Usage:
;;   (elastic-global-toggle)      - Toggle global elastic frame <EL-global>
;;   (elastic-perspective-toggle) - Toggle perspective-local frame <EL-persp-name>
;;
;; In daemon mode, frames are created when first client connects.
;; Works in both GUI and TTY modes (Emacs 31+ supports TTY child frames).

;;; Code:

(require 'cl-lib)

;; Silence byte-compiler warnings for perspective functions
(declare-function persp-current-name "perspective")

;; ============================================================================
;; Configuration
;; ============================================================================

(defgroup elastic nil
  "Elastic child frames for vterm."
  :group 'frames
  :prefix "elastic-")

(defcustom elastic-width-ratio 0.9
  "Width of elastic frame as ratio of parent frame width."
  :type 'float
  :group 'elastic)

(defcustom elastic-height-ratio 0.7
  "Height of elastic frame as ratio of parent frame height."
  :type 'float
  :group 'elastic)

(defcustom elastic-top-offset-ratio 0.15
  "Top offset as ratio of parent frame height."
  :type 'float
  :group 'elastic)

(defcustom elastic-left-offset-ratio 0.05
  "Left offset as ratio of parent frame width."
  :type 'float
  :group 'elastic)

(defcustom elastic-border-width 10
  "Border width for elastic frames."
  :type 'integer
  :group 'elastic)

;; ============================================================================
;; Storage Variables
;; ============================================================================

(defvar elastic--global-frame nil
  "The global elastic vterm frame, shared across all perspectives.")

(defvar elastic--persp-frames (make-hash-table :test 'equal)
  "Hash table mapping perspective names to their elastic frames.")

(defvar elastic--initialized nil
  "Non-nil if elastic has been initialized with a valid parent frame.")

;; ============================================================================
;; Internal Helper Functions
;; ============================================================================

(defun elastic--valid-parent-frame-p (frame)
  "Return non-nil if FRAME can be a parent for elastic child frames.
Returns nil for child frames or invalid frames.
Works in both GUI and TTY modes (Emacs 31+ supports TTY child frames)."
  (and frame
       (frame-live-p frame)
       (not (frame-parent frame))))

(defun elastic--get-parent-frame ()
  "Get appropriate parent frame for elastic frames.
Returns nil if no suitable frame exists (e.g., in daemon mode with no clients).
Works in both GUI and TTY modes."
  (let ((current (selected-frame)))
    (cond
     ;; Current frame is a valid parent frame
     ((elastic--valid-parent-frame-p current)
      current)
     ;; Search for any valid parent frame
     (t
      (cl-find-if #'elastic--valid-parent-frame-p (frame-list))))))

(defun elastic--compute-geometry (parent)
  "Compute child frame geometry based on PARENT frame.
Returns a plist with :width :height :left :top in characters/lines."
  (let* ((parent-width (frame-pixel-width parent))
         (parent-height (frame-pixel-height parent))
         (char-width (frame-char-width parent))
         (char-height (frame-char-height parent))
         ;; Calculate pixel dimensions
         (child-width-px (* elastic-width-ratio parent-width))
         (child-height-px (* elastic-height-ratio parent-height))
         (child-left-px (round (* elastic-left-offset-ratio parent-width)))
         (child-top-px (round (* elastic-top-offset-ratio parent-height)))
         ;; Convert to character dimensions
         (width-chars (max 1 (floor (/ child-width-px char-width))))
         (height-lines (max 1 (floor (/ child-height-px char-height)))))
    (list :width width-chars
          :height height-lines
          :left child-left-px
          :top child-top-px)))

(defun elastic--create-frame (name &optional parent)
  "Create an elastic frame with given NAME.
Uses PARENT as the parent frame, or finds one via `elastic--get-parent-frame'.
Returns the created frame, or nil if no valid parent exists."
  (let ((parent-frame (or parent (elastic--get-parent-frame))))
    (unless parent-frame
      (user-error "No valid parent frame available for elastic frame"))
    (let* ((geometry (elastic--compute-geometry parent-frame))
           (frame-params `((parent-frame . ,parent-frame)
                           (title . ,name)
                           (name . ,name)
                           (left . ,(plist-get geometry :left))
                           (top . ,(plist-get geometry :top))
                           (width . ,(plist-get geometry :width))
                           (height . ,(plist-get geometry :height))
                           (child-frame-border-width . ,elastic-border-width)
                           (minibuffer . t)
                           (visibility . t)
                           (no-accept-focus . nil)
                           (undecorated . nil)
                           (keep-ratio . t)
                           (no-other-frame . t)
                           (no-special-glyphs . t)
                           (desktop-dont-save . t)))
           (frame (make-frame frame-params)))
      ;; Setup vterm in the new frame
      (elastic--setup-vterm-buffer frame)
      ;; Focus the new frame
      (select-frame-set-input-focus frame)
      frame)))

(defun elastic--setup-vterm-buffer (frame)
  "Set up a vterm buffer in FRAME.
Disables mode-line and line numbers, sets process query flag to nil."
  (when (fboundp 'vterm)
    (with-selected-frame frame
      (let ((vterm-buffer (vterm)))
        (set-window-buffer (frame-root-window frame) vterm-buffer)
        (with-current-buffer vterm-buffer
          ;; Disable mode-line
          (setq-local mode-line-format nil)
          ;; Disable line numbers
          (when (boundp 'display-line-numbers)
            (setq-local display-line-numbers nil))
          (when (fboundp 'display-line-numbers-mode)
            (display-line-numbers-mode -1))
          ;; Don't ask for confirmation when killing
          (when-let* ((proc (get-buffer-process (current-buffer))))
            (set-process-query-on-exit-flag proc nil)))))))

(defun elastic--toggle-frame (getter setter name-fn)
  "Generic toggle logic for elastic frames.
GETTER: function returning current frame (or nil).
SETTER: function to store created frame (called with frame arg).
NAME-FN: function returning frame name string."
  (let ((frame (funcall getter)))
    (cond
     ;; Frame exists and is visible -> hide it
     ((and frame (frame-live-p frame) (frame-visible-p frame))
      (make-frame-invisible frame))
     ;; Frame exists but hidden -> show and focus
     ((and frame (frame-live-p frame))
      (make-frame-visible frame)
      (select-frame-set-input-focus frame))
     ;; Frame doesn't exist or is dead -> create new one
     (t
      (let ((new-frame (elastic--create-frame (funcall name-fn))))
        (when new-frame
          (funcall setter new-frame)))))))

;; ============================================================================
;; Perspective Helpers
;; ============================================================================

(defun elastic--current-persp-name ()
  "Return current perspective name, or \"main\" if persp-mode is disabled."
  (if (bound-and-true-p persp-mode)
      (persp-current-name)
    "main"))

;; ============================================================================
;; Interactive Commands
;; ============================================================================

;;;###autoload
(defun elastic-global-toggle ()
  "Toggle the global elastic vterm frame.
The global frame is shared across all perspectives and named <EL-global>."
  (interactive)
  (elastic--toggle-frame
   (lambda () elastic--global-frame)
   (lambda (f) (setq elastic--global-frame f))
   (lambda () "<EL-global>")))

;;;###autoload
(defun elastic-perspective-toggle ()
  "Toggle elastic vterm frame for current perspective.
Each perspective has its own frame, named <EL-perspective-name>."
  (interactive)
  (let ((persp-name (elastic--current-persp-name)))
    (elastic--toggle-frame
     (lambda () (gethash persp-name elastic--persp-frames))
     (lambda (f) (puthash persp-name f elastic--persp-frames))
     (lambda () (format "<EL-%s>" persp-name)))))

;; ============================================================================
;; Cleanup Hooks
;; ============================================================================

(defun elastic--on-frame-deleted (frame)
  "Handle FRAME deletion, removing it from storage if it's an elastic frame."
  ;; Check if it's the global frame
  (when (and elastic--global-frame
             (eq frame elastic--global-frame))
    (setq elastic--global-frame nil))
  ;; Check if it's a perspective frame
  (maphash (lambda (persp-name persp-frame)
             (when (eq frame persp-frame)
               (remhash persp-name elastic--persp-frames)))
           elastic--persp-frames))

(defun elastic--on-persp-killed ()
  "Handle perspective deletion, removing its elastic frame."
  (let* ((persp-name (elastic--current-persp-name))
         (frame (gethash persp-name elastic--persp-frames)))
    (when (and frame (frame-live-p frame))
      (delete-frame frame))
    (remhash persp-name elastic--persp-frames)))

;; ============================================================================
;; Initialization
;; ============================================================================

(defun elastic--maybe-init (&optional frame)
  "Initialize elastic when first valid parent frame is available.
Called from `after-make-frame-functions' with FRAME argument."
  (when (and (not elastic--initialized)
             (elastic--valid-parent-frame-p (or frame (selected-frame))))
    (setq elastic--initialized t)))

(defun elastic--setup-hooks ()
  "Set up all hooks for elastic frame management."
  ;; Daemon-safe initialization
  (add-hook 'after-make-frame-functions #'elastic--maybe-init)
  ;; Cleanup when frames are deleted
  (add-hook 'delete-frame-functions #'elastic--on-frame-deleted)
  ;; Cleanup when perspectives are killed
  (with-eval-after-load 'perspective
    (add-hook 'persp-killed-hook #'elastic--on-persp-killed)))

;; Initialize hooks on load
(elastic--setup-hooks)

;; Try to initialize now if a GUI frame already exists
(elastic--maybe-init)

(provide 'elastic)
;;; elastic.el ends here
