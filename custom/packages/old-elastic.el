;;; elastic.el --- Child-frames control -*- lexical-binding: t; -*-

;;; Commentary:
;; Package to control child frames and theirs content.

;;; Code:

;; ==================================
;; Debug Configuration
;; ==================================

(defvar elastic-debug-enabled (getenv "EMACS_ELASTIC_DEBUG")
  "Enable debug logging for elastic frame.
Set environment variable EMACS_ELASTIC_DEBUG=1 to enable.")

(defvar elastic-debug-log-file (expand-file-name "~/.emacs.d/logs/elastic-debug.log")
  "Path to the elastic debug log file.")

(defun elastic--debug-log (format-string &rest args)
  "Log a debug message to the elastic debug log file.
FORMAT-STRING and ARGS are passed to `format'."
  (when elastic-debug-enabled
    (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
          (message (apply #'format format-string args)))
      (with-temp-buffer
        (insert (format "[%s] %s\n" timestamp message))
        (append-to-file (point-min) (point-max) elastic-debug-log-file)))))

(defvar elastic-vterm--frame nil
  "The floating vterm frame used by elastic-vterm.
This frame is reused when toggling the terminal.")

(defvar floating-frame--parent nil
  "The parent frame whose geometry is used as reference for floating frames.
Initialized lazily when first needed.")

(defvar floating-frame--parent-geometry nil
  "A plist storing the current pixel geometry of `floating-frame--parent'.
The keys are :width and :height. Initialized lazily when first needed.")

(defun floating-frame--get-parent ()
  "Get the appropriate parent frame for child frames.
Returns the stored parent frame if it's still live, otherwise
returns the currently selected frame and updates the stored reference."
  (if (and (framep floating-frame--parent)
           (frame-live-p floating-frame--parent))
      floating-frame--parent
    (setq floating-frame--parent (selected-frame))))

(defun floating-frame--update-geometry (frame)
  "Update the stored geometry for FRAME if it is the parent frame.
This function is meant to be used in `after-change-frame-geometry-hook'."
  (when (and floating-frame--parent (eq frame floating-frame--parent))
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
  (elastic--debug-log "========================================")
  (elastic--debug-log "ELASTIC-VTERM INVOKED")
  (elastic--debug-log "========================================")
  (elastic--debug-log "Display type: %s" (if (display-graphic-p) "GUI" "TTY"))
  (elastic--debug-log "Emacs version: %s" emacs-version)
  (elastic--debug-log "System: %s" system-type)
  
  ;; CRITICAL FIX: Destroy old frame if it exists to avoid geometry conflicts
  (when (and elastic-vterm--frame (frame-live-p elastic-vterm--frame))
    (elastic--debug-log "Destroying existing elastic-vterm--frame")
    (delete-frame elastic-vterm--frame)
    (setq elastic-vterm--frame nil))
  
  (let* ((parent (floating-frame--get-parent))
         ;; Use native dimensions for more accurate pixel calculations
         (parent-native-width (frame-native-width parent))
         (parent-native-height (frame-native-height parent))
         (parent-pixel-width (frame-pixel-width parent))
         (parent-pixel-height (frame-pixel-height parent))
         (parent-char-width (frame-char-width parent))
         (parent-char-height (frame-char-height parent))
         
         ;; Get parent position for debugging
         (parent-left (frame-parameter parent 'left))
         (parent-top (frame-parameter parent 'top)))
    
    (elastic--debug-log "")
    (elastic--debug-log "--- PARENT FRAME INFO ---")
    (elastic--debug-log "Parent frame: %s" parent)
    (elastic--debug-log "Native dimensions: %dx%d pixels" parent-native-width parent-native-height)
    (elastic--debug-log "Pixel dimensions: %dx%d pixels" parent-pixel-width parent-pixel-height)
    (elastic--debug-log "Char dimensions: %dx%d (w x h)" parent-char-width parent-char-height)
    (elastic--debug-log "Position: left=%s top=%s" parent-left parent-top)
    
    ;; CRITICAL FIX: Use native pixel dimensions for accurate calculations in GUI mode
    (let* ((use-native (display-graphic-p))
           (base-width (if use-native parent-native-width parent-pixel-width))
           (base-height (if use-native parent-native-height parent-pixel-height))
           
           ;; Calculate the desired child frame dimensions in pixels.
           (child-width-pixels (* 0.9 base-width))
           (child-height-pixels (* 0.7 base-height))
           (child-left (round (* 0.05 base-width)))
           (child-top (round (* 0.15 base-height)))
           
           ;; Convert pixel dimensions to text dimensions.
           (char-width parent-char-width)
           (char-height parent-char-height)
           (child-width-chars (max 1 (floor (/ child-width-pixels char-width))))
           (child-height-lines (max 1 (floor (/ child-height-pixels char-height)))))
      
      (elastic--debug-log "")
      (elastic--debug-log "--- CALCULATION DETAILS ---")
      (elastic--debug-log "Using native dimensions: %s" use-native)
      (elastic--debug-log "Base dimensions: %dx%d" base-width base-height)
      (elastic--debug-log "Child size (pixels): %dx%d" child-width-pixels child-height-pixels)
      (elastic--debug-log "Child position (pixels): left=%d top=%d" child-left child-top)
      (elastic--debug-log "Character cell size: %dx%d" char-width char-height)
      (elastic--debug-log "Child size (chars): %dx%d" child-width-chars child-height-lines)
      
      ;; CRITICAL FIX: Remove keep-ratio and add user-position/user-size
      ;; The keep-ratio parameter causes Emacs to override our geometry!
      (let ((frame-params `((parent-frame . ,parent)
                            (title . "elastic-vterm")
                            (left . ,child-left)
                            (top . ,child-top)
                            (width . ,child-width-chars)
                            (height . ,child-height-lines)
                            (child-frame-border-width . 10)
                            (visibility . t)
                            (no-accept-focus . nil)
                            (undecorated . nil)
                            ;; REMOVED: (keep-ratio . t)
                            ;; This was causing Emacs to override our geometry!
                            (user-position . t)  ; Respect our position
                            (user-size . t)      ; Respect our size
                            (persp-ignore-wconf . t)))) ; Don't let persp-mode save/restore wconf
        
        (elastic--debug-log "")
        (elastic--debug-log "--- FRAME PARAMETERS ---")
        (elastic--debug-log "%S" frame-params)
        
        (setq elastic-vterm--frame (make-frame frame-params))
        
        ;; Log actual frame dimensions after creation
        (elastic--debug-log "")
        (elastic--debug-log "--- CREATED FRAME INFO ---")
        (elastic--debug-log "Child frame: %s" elastic-vterm--frame)
        (elastic--debug-log "Actual native dimensions: %dx%d"
                            (frame-native-width elastic-vterm--frame)
                            (frame-native-height elastic-vterm--frame))
        (elastic--debug-log "Actual pixel dimensions: %dx%d"
                            (frame-pixel-width elastic-vterm--frame)
                            (frame-pixel-height elastic-vterm--frame))
        (elastic--debug-log "Actual position: left=%s top=%s"
                            (frame-parameter elastic-vterm--frame 'left)
                            (frame-parameter elastic-vterm--frame 'top))
        
        ;; Calculate differences
        (let ((actual-width (frame-pixel-width elastic-vterm--frame))
              (actual-height (frame-pixel-height elastic-vterm--frame))
              (expected-width (round child-width-pixels))
              (expected-height (round child-height-pixels)))
          (elastic--debug-log "")
          (elastic--debug-log "--- COMPARISON ---")
          (elastic--debug-log "Width:  expected=%d actual=%d diff=%d (%.1f%%)"
                              expected-width actual-width (- actual-width expected-width)
                              (* 100.0 (/ (float (- actual-width expected-width)) expected-width)))
          (elastic--debug-log "Height: expected=%d actual=%d diff=%d (%.1f%%)"
                              expected-height actual-height (- actual-height expected-height)
                              (* 100.0 (/ (float (- actual-height expected-height)) expected-height)))
          
          (when (or (> (abs (- actual-width expected-width)) 50)
                    (> (abs (- actual-height expected-height)) 50))
            (elastic--debug-log "WARNING: Significant geometry mismatch detected!"))
          
          (elastic--debug-log "========================================")
          (elastic--debug-log ""))
        
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
                (set-process-query-on-exit-flag proc nil)))))))))

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
