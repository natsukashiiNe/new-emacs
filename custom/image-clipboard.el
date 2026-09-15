;;; image-clipboard.el --- Copy images to system clipboard -*- lexical-binding: t; -*-

;;; Commentary:
;; Push the image displayed in the current buffer (image-mode,
;; telega-image-mode, or any derivative) onto the X11 system clipboard
;; via `xclip', with the correct MIME type for the source format.

;;; Code:

(defvar my/image-clipboard-program "xclip"
  "Program used to push images onto the system clipboard.")

(defconst my/image-clipboard-mime-alist
  '(("png"  . "image/png")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif"  . "image/gif")
    ("bmp"  . "image/bmp")
    ("webp" . "image/webp")
    ("svg"  . "image/svg+xml")
    ("tif"  . "image/tiff")
    ("tiff" . "image/tiff"))
  "Mapping from lowercased file extensions to MIME types.")

(defun my/image--mime-type (file)
  "Return the MIME type string for FILE based on its extension."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (or (cdr (assoc ext my/image-clipboard-mime-alist))
        "image/png")))

(defun my/image--current-file ()
  "Return (FILE . CLEANUP) for the image in the current buffer.
CLEANUP is a function to delete a temp file once xclip finishes,
or nil when no cleanup is needed."
  (cond
   ;; Buffers visiting an image file (image-mode, telega-image-mode, …).
   ((and buffer-file-name (file-exists-p buffer-file-name))
    (cons buffer-file-name nil))
   ;; In-memory image data (e.g. images decoded from a stream).
   ((derived-mode-p 'image-mode)
    (let* ((img  (image-get-display-property))
           (file (and (consp img) (plist-get (cdr img) :file)))
           (data (and (consp img) (plist-get (cdr img) :data)))
           (type (and (consp img) (plist-get (cdr img) :type))))
      (cond
       ((and file (file-exists-p file)) (cons file nil))
       (data
        (let* ((ext (if type (symbol-name type) "png"))
               (tmp (make-temp-file "emacs-img-" nil (concat "." ext))))
          (let ((coding-system-for-write 'binary))
            (with-temp-file tmp (insert data)))
          (cons tmp (lambda () (ignore-errors (delete-file tmp))))))
       (t (user-error "No image file or data in this buffer")))))
   (t (user-error "Not an image buffer"))))

;;;###autoload
(defun my/copy-image-to-clipboard ()
  "Copy the image in the current buffer to the system clipboard.
Uses `xclip' under X11. Works in `image-mode' and derivatives such
as `telega-image-mode'."
  (interactive)
  (unless (executable-find my/image-clipboard-program)
    (user-error "%s not found on PATH" my/image-clipboard-program))
  (pcase-let* ((`(,file . ,cleanup) (my/image--current-file))
               (mime (my/image--mime-type file))
               (proc (make-process
                      :name "image-xclip"
                      :buffer nil
                      :noquery t
                      :command (list my/image-clipboard-program
                                     "-selection" "clipboard"
                                     "-t" mime
                                     "-i" file)
                      :sentinel
                      (lambda (_proc _event)
                        (when cleanup (funcall cleanup))))))
    (message "Copied %s (%s) to clipboard"
             (file-name-nondirectory file) mime)
    proc))

;;;###autoload
(defun my/copy-image-path-to-clipboard ()
  "Copy the path of the image in the current buffer to the system clipboard."
  (interactive)
  (pcase-let ((`(,file . ,_) (my/image--current-file)))
    (kill-new file)
    (when (executable-find my/image-clipboard-program)
      (let ((proc (make-process
                   :name "image-path-xclip"
                   :buffer nil
                   :noquery t
                   :command (list my/image-clipboard-program
                                  "-selection" "clipboard"
                                  "-i"))))
        (process-send-string proc file)
        (process-send-eof proc)))
    (message "Copied path: %s" file)))

(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "C-c w") #'my/copy-image-to-clipboard)
  (define-key image-mode-map (kbd "C-c W") #'my/copy-image-path-to-clipboard))

(with-eval-after-load 'evil
  (with-eval-after-load 'image-mode
    (evil-define-key 'normal image-mode-map
      (kbd "Y") #'my/copy-image-to-clipboard)))

(with-eval-after-load 'telega-modes
  (when (boundp 'telega-image-mode-map)
    (define-key telega-image-mode-map (kbd "C-c w") #'my/copy-image-to-clipboard)
    (define-key telega-image-mode-map (kbd "C-c W") #'my/copy-image-path-to-clipboard)))

(provide 'image-clipboard)
;;; image-clipboard.el ends here
