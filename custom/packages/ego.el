;;; ego.el --- Personal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Usage in your config:
;;
;;   (require 'ego)
;;
;;   (setq ego-ql-locations
;;     '(("d"   . "~/Downloads")
;;       ("h s" . "~/Pictures/screenshots")
;;       ("p"   . "~/Projects")))
;;
;;   ;; Optionally override defaults:
;;   ;; (setq ego-ql-global-prefix "C-c G")
;;   ;; (setq ego-ql-dired-prefix "M-f")
;;   ;; (setq ego-ql-minibuffer-prefix "C-c l")
;;
;;   (ego-ql-setup)

;;; Code:

(require 'cl-lib)

;;;; User-facing variables

(defvar ego-ql-locations nil
  "Alist of (KEY-SEQUENCE . PATH) for quick location access.
KEY-SEQUENCE is a string like \"d\" or \"h s\" (space-separated keys).")

(defvar ego-ql-global-prefix nil
  "Key sequence string for the global quick-locations prefix (e.g. \"C-c G\").")

(defvar ego-ql-dired-prefix nil
  "Key sequence string for the dirvish/dired-mode quick-locations prefix (e.g. \"M-f\").")

(defvar ego-ql-minibuffer-prefix nil
  "Key sequence string for the minibuffer quick-locations prefix (e.g. \"C-c l\").")

;;;; Internal variables

(defvar ego-ql--dired-command
  (if (featurep 'dirvish) #'dirvish #'dired)
  "Command used to open directories.")

(defvar ego-ql--map (make-sparse-keymap)
  "Keymap for jumping to directories via `ego-ql--dired-command'.")

(defvar ego-ql--minibuffer-map (make-sparse-keymap)
  "Keymap for inserting paths into the minibuffer.")

;;;; Internal helpers

(defun ego-ql--make-dired-action (path)
  "Return a command that opens `ego-ql--dired-command' at PATH."
  (let ((expanded-path (expand-file-name path)))
    (lambda ()
      (interactive)
      (funcall ego-ql--dired-command expanded-path))))

(defun ego-ql--make-minibuffer-action (path)
  "Return a command that replaces minibuffer contents with PATH."
  (let ((expanded-path (expand-file-name path)))
    (lambda ()
      (interactive)
      (delete-minibuffer-contents)
      (insert expanded-path))))

(defun ego-ql--bind-to-map (keymap key-str make-action-fn)
  "Bind KEY-STR in KEYMAP using MAKE-ACTION-FN applied to the matching path.
KEY-STR can be \"d\" or multi-key like \"h s\"."
  (let* ((path (cdr (assoc key-str ego-ql-locations)))
         (keys (split-string key-str " "))
         (cmd (funcall make-action-fn path)))
    (if (= (length keys) 1)
        (define-key keymap (kbd key-str) cmd)
      (let ((prefix-keys (butlast keys))
            (final-key (car (last keys)))
            (current-map keymap))
        (dolist (k prefix-keys)
          (let ((existing (lookup-key current-map (kbd k))))
            (unless (keymapp existing)
              (let ((new-map (make-sparse-keymap)))
                (define-key current-map (kbd k) new-map)
                (setq existing new-map)))
            (setq current-map existing)))
        (define-key current-map (kbd final-key) cmd)))))

(defun ego-ql--build-maps ()
  "Rebuild `ego-ql--map' and `ego-ql--minibuffer-map' from `ego-ql-locations'."
  (setq ego-ql--map (make-sparse-keymap))
  (setq ego-ql--minibuffer-map (make-sparse-keymap))
  (dolist (entry ego-ql-locations)
    (let ((key-str (car entry)))
      (ego-ql--bind-to-map ego-ql--map key-str #'ego-ql--make-dired-action)
      (ego-ql--bind-to-map ego-ql--minibuffer-map key-str #'ego-ql--make-minibuffer-action))))

;;;; Public setup

(defun ego-ql-setup ()
  "Set up quick-locations keybindings from `ego-ql-locations'.

Binds prefixes only when the corresponding variable is non-nil:
  `ego-ql-global-prefix'     - global prefix to the locations map
  `ego-ql-dired-prefix'      - prefix in dirvish/dired mode map
  `ego-ql-minibuffer-prefix'  - prefix in all minibuffer maps (shadows global)"
  (interactive)
  (ego-ql--build-maps)

  (when ego-ql-global-prefix
    (global-set-key (kbd ego-ql-global-prefix) ego-ql--map))

  (when ego-ql-minibuffer-prefix
    (dolist (map (list minibuffer-local-map
                       minibuffer-local-completion-map
                       (when (boundp 'minibuffer-local-filename-completion-map)
                         minibuffer-local-filename-completion-map)))
      (when map
        (define-key map (kbd ego-ql-minibuffer-prefix) ego-ql--minibuffer-map))))

  (when ego-ql-dired-prefix
    (if (featurep 'dirvish)
        (define-key dirvish-mode-map (kbd ego-ql-dired-prefix) ego-ql--map)
      (with-eval-after-load 'dirvish
        (define-key dirvish-mode-map (kbd ego-ql-dired-prefix) ego-ql--map)))))

(provide 'ego)
;;; ego.el ends here
