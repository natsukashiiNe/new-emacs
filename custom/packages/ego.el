;;; ego.el --- Personal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Usage in your config:
;;
;;   (require 'ego)
;;
;;   (setq ego-ql-locations
;;     '(;; Simple cons pairs — desc defaults to "eql <path>":
;;       ("d"   . "~/Downloads")
;;       ("h s" . "~/Pictures/screenshots")
;;
;;       ;; Plist with custom desc — @path expands to the :path value:
;;       (:key "p" :path "~/Projects" :desc "ego @path")
;;
;;       ;; Plist without :desc — defaults to "eql <path>":
;;       (:key "c" :path "~/.config")
;;
;;       ;; Both formats can be mixed freely in the same list.
;;       ))
;;
;;   ;; Optionally override defaults:
;;   ;; (setq ego-ql-global-prefix "C-c G")
;;   ;; (setq ego-ql-dired-prefix "M-f")
;;   ;; (setq ego-ql-minibuffer-prefix "C-c l")
;;
;;   (ego-ql-setup)
;;
;; Description templates:
;;   @path  — expands to the :path value
;;
;; Examples:
;;   (:key "d" :path "~/Downloads" :desc "ego @path")
;;     => which-key shows: "ego ~/Downloads"
;;
;;   (:key "s" :path "~/src" :desc "sources")
;;     => which-key shows: "sources"
;;
;;   (:key "n" :path "~/Notes")
;;     => which-key shows: "eql ~/Notes"
;;
;;   ("t" . "/tmp")
;;     => which-key shows: "eql /tmp"

;;; Code:

(require 'cl-lib)

;;;; User-facing variables

(defvar ego-ql-locations nil
  "List of quick-location entries.
Each entry is either a cons pair (KEY . PATH) or a plist
\(:key KEY :path PATH [:desc DESC]).  DESC supports @path
expansion.  Default description is \"eql @path\".")

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

(defun ego-ql--normalize-entry (entry)
  "Normalize ENTRY to (:key KEY :path PATH :desc DESC).
ENTRY is either (KEY . PATH) or (:key KEY :path PATH [:desc DESC]).
@path in DESC expands to PATH.  Default desc is \"eql @path\"."
  (let (key path desc)
    (if (keywordp (car entry))
        (setq key  (plist-get entry :key)
              path (plist-get entry :path)
              desc (or (plist-get entry :desc) "eql @path"))
      (setq key  (car entry)
            path (cdr entry)
            desc "eql @path"))
    (setq desc (string-replace "@path" path desc))
    (list :key key :path path :desc desc)))

(defun ego-ql--normalize-locations ()
  "Normalize `ego-ql-locations' into a list of canonical plists."
  (mapcar #'ego-ql--normalize-entry ego-ql-locations))

(defun ego-ql--bind-to-map (keymap key-str path desc make-action-fn)
  "Bind KEY-STR in KEYMAP to a command created by MAKE-ACTION-FN for PATH.
DESC is shown in which-key.  KEY-STR can be \"d\" or multi-key like \"h s\"."
  (let* ((keys (split-string key-str " "))
         (cmd (cons desc (funcall make-action-fn path))))
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
  (dolist (entry (ego-ql--normalize-locations))
    (let ((key  (plist-get entry :key))
          (path (plist-get entry :path))
          (desc (plist-get entry :desc)))
      (ego-ql--bind-to-map ego-ql--map key path desc #'ego-ql--make-dired-action)
      (ego-ql--bind-to-map ego-ql--minibuffer-map key path desc #'ego-ql--make-minibuffer-action))))

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
