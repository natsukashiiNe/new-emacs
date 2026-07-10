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

(defvar elastic-debug-log-file (expand-file-name "/tmp/log/emacs/elastic.log")
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

(defvar elastic-vterm--frames (make-hash-table :test 'eq :weakness 'key)
  "Map of parent top-level frame -> its elastic-vterm child frame.
Weak on keys so dead parent frames are GC'd automatically.")

(defun elastic--toplevel-frame (&optional frame)
  "Return the top-level ancestor of FRAME (or selected frame).
Walks up `parent-frame' until a top-level frame is reached."
  (let ((f (or frame (selected-frame))))
    (while (frame-parameter f 'parent-frame)
      (setq f (frame-parameter f 'parent-frame)))
    f))

(defun elastic--current-parent ()
  "Return the top-level frame that should host an elastic child right now."
  (elastic--toplevel-frame (selected-frame)))

(defun elastic--cleanup-on-delete (frame)
  "Tear down elastic children attached to FRAME before FRAME is deleted.
Hooked into `delete-frame-functions' so that closing a parent frame
does not fail with `Attempt to delete a surrogate minibuffer frame'
and does not leave orphaned child frames behind."
  (let (to-remove)
    (maphash
     (lambda (parent child)
       (when (or (eq parent frame)
                 (and (frame-live-p child)
                      (eq (frame-parameter child 'parent-frame) frame)))
         (when (frame-live-p child)
           (delete-frame child t))
         (push parent to-remove)))
     elastic-vterm--frames)
    (dolist (p to-remove)
      (remhash p elastic-vterm--frames))))

(add-hook 'delete-frame-functions #'elastic--cleanup-on-delete)

;;;###autoload
(defun elastic-vterm ()
  "Launch a vterm in a new floating child frame for the current parent.
The new frame's dimensions are computed as 70% of the parent frame's size,
with a 15% offset from the parent's left and top edges.
The vterm buffer has its mode-line and line numbers disabled.
The child is stored in `elastic-vterm--frames' keyed by its top-level
parent frame so each Emacs client/frame gets its own elastic vterm."
  (interactive)
  (elastic--debug-log "========================================")
  (elastic--debug-log "ELASTIC-VTERM INVOKED")
  (elastic--debug-log "========================================")
  (elastic--debug-log "Display type: %s" (if (display-graphic-p) "GUI" "TTY"))
  (elastic--debug-log "Emacs version: %s" emacs-version)
  (elastic--debug-log "System: %s" system-type)

  ;; Capture parent ONCE before any delete-frame call.  If we called
  ;; elastic--current-parent a second time after deleting the old child,
  ;; X11 can redirect focus to a different frame (e.g. the daemon's
  ;; invisible initial frame), giving completely wrong pixel dimensions.
  (let* ((parent (elastic--current-parent))
         (existing (gethash parent elastic-vterm--frames)))
    (when (and existing (frame-live-p existing))
      (elastic--debug-log "Destroying existing elastic child for parent %s" parent)
      (delete-frame existing t))
    (remhash parent elastic-vterm--frames)

    (let* (;; Snapshot font/fringe from the PARENT frame (which has the correct
           ;; X-display metrics).  Passing these into make-frame ensures the
           ;; daemon creates the child with the right char-cell dimensions from
           ;; the start.  Without this, the daemon uses its own headless metrics
           ;; (char_w≈9px instead of 17px), then after-make-frame-functions
           ;; fires, set-face-attribute remeasures on the real display, and
           ;; Emacs resizes the frame to keep the column count — blowing it up
           ;; by ~1.9x.
           (parent-font        (face-font 'default parent))
           (parent-left-fringe  (or (frame-parameter parent 'left-fringe)
                                    (nth 0 (window-fringes (frame-root-window parent)))
                                    16))
           (parent-right-fringe (or (frame-parameter parent 'right-fringe)
                                    (nth 1 (window-fringes (frame-root-window parent)))
                                    16))
           ;; Column/row count from the parent (measured with correct char
           ;; metrics after its font was set).
           (parent-cols (frame-width  parent))
           (parent-rows (frame-height parent))
           ;; Position in logical X11 pixels — the same space child left/top use.
           (base-width  (frame-pixel-width  parent))
           (base-height (frame-pixel-height parent))
           (parent-char-width  (frame-char-width  parent))
           (parent-char-height (frame-char-height parent))
           (parent-left (frame-parameter parent 'left))
           (parent-top  (frame-parameter parent 'top)))

      (elastic--debug-log "")
      (elastic--debug-log "--- PARENT FRAME INFO ---")
      (elastic--debug-log "Parent frame: %s" parent)
      (elastic--debug-log "Native dimensions: %dx%d pixels"
                          (frame-native-width parent) (frame-native-height parent))
      (elastic--debug-log "Pixel dimensions: %dx%d  Char dimensions: %dx%d"
                          base-width base-height parent-char-width parent-char-height)
      (elastic--debug-log "Cols x rows: %dx%d" parent-cols parent-rows)
      (elastic--debug-log "Position: left=%s top=%s" parent-left parent-top)

      (let* ((child-cols (round (* 0.85 parent-cols)))
             (child-rows (round (* 0.8  parent-rows)))
             (child-left (round (* 0.055 base-width)))
             (child-top  (round (* 0.09  base-height))))

        (elastic--debug-log "")
        (elastic--debug-log "--- CALCULATION DETAILS ---")
        (elastic--debug-log "Child size (cols x rows): %dx%d" child-cols child-rows)
        (elastic--debug-log "Child position (pixels): left=%d top=%d" child-left child-top)

        (let ((frame-params `((parent-frame . ,parent)
                              (title . "elastic-vterm")
                              (left . ,child-left)
                              (top . ,child-top)
                              (width  . ,child-cols)
                              (height . ,child-rows)
                              ;; Mirror the parent's font and fringe so that
                              ;; after-make-frame-functions (which calls
                              ;; set-face-attribute + set-fringe-mode) finds
                              ;; them already correct and triggers no resize.
                              (font         . ,parent-font)
                              (left-fringe  . ,parent-left-fringe)
                              (right-fringe . ,parent-right-fringe)
                              (child-frame-border-width . 10)
                              (visibility . t)
                              (no-accept-focus . nil)
                              (undecorated . nil)
                              (minibuffer . t)
                              (user-position . t)
                              (user-size . t)
                              (persp-ignore-wconf . t))))

          (elastic--debug-log "")
          (elastic--debug-log "--- FRAME PARAMETERS ---")
          (elastic--debug-log "%S" frame-params)

          (let ((child (make-frame frame-params)))
            (puthash parent child elastic-vterm--frames)

            ;; Log actual frame dimensions after creation
            (elastic--debug-log "")
            (elastic--debug-log "--- CREATED FRAME INFO ---")
            (elastic--debug-log "Child frame: %s" child)
            (elastic--debug-log "Actual native dimensions: %dx%d"
				(frame-native-width child)
				(frame-native-height child))
            (elastic--debug-log "Actual pixel dimensions: %dx%d"
				(frame-pixel-width child)
				(frame-pixel-height child))
            (elastic--debug-log "Actual position: left=%s top=%s"
				(frame-parameter child 'left)
				(frame-parameter child 'top))

            ;; Calculate differences
            (let ((actual-width (frame-pixel-width child))
                  (actual-height (frame-pixel-height child))
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

            (select-frame-set-input-focus child)

            ;; Launch vterm in the new frame if available.
            (when (fboundp 'multi-vterm)
              (let* ((proj (project-current))
                     (default-directory (if proj
                                            (project-root proj)
                                          default-directory))
                     (vterm-buffer (multi-vterm)))
		(set-window-buffer (frame-root-window child) vterm-buffer)
		;; Tag window for buffer routing
		(set-window-parameter (frame-root-window child)
                                      'emux-slot "elastic-vterm")
		(with-current-buffer vterm-buffer
                  (when (boundp 'display-line-numbers)
                    (setq-local display-line-numbers nil))
                  (when (fboundp 'display-line-numbers-mode)
                    (display-line-numbers-mode -1))
                  ;; Disable the exit confirmation by clearing the process query flag.
                  (when-let ((proc (get-buffer-process (current-buffer))))
                    (set-process-query-on-exit-flag proc nil)))))))))))

;;;###autoload
(defun elastic-vterm-toggle ()
  "Toggle the visibility of the elastic vterm frame for the current parent.
Each top-level parent frame has its own elastic child, looked up in
`elastic-vterm--frames'.  If the child for the current parent exists
and is live, hide it if visible or show it if hidden.  If it doesn't
exist, create one using `elastic-vterm'."
  (interactive)
  (let* ((parent (elastic--current-parent))
         (child  (gethash parent elastic-vterm--frames)))
    (if (and child (frame-live-p child))
        (if (frame-visible-p child)
            (make-frame-invisible child)
          (make-frame-visible child)
          (select-frame-set-input-focus child))
      (elastic-vterm))))

(provide 'elastic)
;;; elastic.el ends here
