;;; consult-buffer-by-mode.el --- Consult picker filtered by major-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Generalizes the by-mode buffer-picking pattern (see
;; `ego-project-buffers-by-mode') from a single project's buffers to all
;; live buffers, with consult preview.

;;; Code:

(require 'cl-lib)
(require 'consult)

(defun my-consult/buffer-by-mode (mode &optional prompt)
  "Pick a live buffer whose `major-mode' is MODE, previewing candidates.
If exactly one such buffer exists, return it without prompting.
PROMPT overrides the default minibuffer prompt.  Signals a `user-error'
when no buffer with MODE exists."
  (let* ((bufs (cl-remove-if-not
                (lambda (b) (eq (buffer-local-value 'major-mode b) mode))
                (buffer-list)))
         (names (mapcar #'buffer-name bufs)))
    (cond
     ((null names) (user-error "No buffer with major-mode %s" mode))
     ((= (length names) 1) (car bufs))
     (t (get-buffer
         (consult--read names
                         :prompt (or prompt (format "%s buffer: " mode))
                         :category 'buffer
                         :state (consult--buffer-state)))))))

(provide 'consult-buffer-by-mode)
;;; consult-buffer-by-mode.el ends here
