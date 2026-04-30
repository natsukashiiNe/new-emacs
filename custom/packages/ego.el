;;; ego.el --- Personal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Usage in your config:
;;
;;   (require 'ego)
;;
;;   (setq ego-ql-locations
;;     '(;; Prefix-only label (no :path — just a which-key group label):
;;       (:key "e"   :label "Emacs")
;;
;;       ;; Leaf binding with which-key description:
;;       (:key "e s" :path "/run/user/1000/emacs" :wk "sockets")
;;
;;       ;; Leaf without :wk — defaults to the :path value:
;;       (:key "c"   :path "~/.config")
;;
;;       ;; Custom :wk description:
;;       (:key "R"   :path "/"          :wk "root (/)")
;;       ))
;;
;;   ;; Optionally override defaults:
;;   ;; (setq ego-ql-global-prefix "C-c G")
;;   ;; (setq ego-ql-dired-prefix "M-f")
;;   ;; (setq ego-ql-minibuffer-prefix "C-c l")
;;
;;   (ego-ql-setup)
;;
;; Entry format:
;;   (:key KEY :path PATH [:wk DESC])   — bind KEY to open PATH
;;   (:key KEY :label LABEL)            — prefix-only which-key label
;;
;; :wk defaults to PATH when omitted.

;;; Code:

(require 'cl-lib)

;;;; User-facing variables

(defvar ego-ql-locations nil
  "List of quick-location entries.
Each entry is a plist:
  (:key KEY :path PATH [:wk DESC])   — bind KEY to open PATH
  (:key KEY :label LABEL)            — prefix-only which-key label
:wk defaults to PATH when omitted.")

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

(defun ego-ql--leaf-entries ()
  "Return leaf entries from `ego-ql-locations' (those with :path)."
  (cl-remove-if-not (lambda (e) (plist-get e :path)) ego-ql-locations))

(defun ego-ql--prefix-entries ()
  "Return prefix-only entries from `ego-ql-locations' (those with :label)."
  (cl-remove-if-not (lambda (e) (plist-get e :label)) ego-ql-locations))

(defun ego-ql--entry-wk (entry)
  "Return the which-key description for ENTRY, defaulting to :path."
  (or (plist-get entry :wk) (plist-get entry :path)))

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
  ;; Bind leaf entries
  (dolist (entry (ego-ql--leaf-entries))
    (let ((key  (plist-get entry :key))
          (path (plist-get entry :path))
          (wk   (ego-ql--entry-wk entry)))
      (ego-ql--bind-to-map ego-ql--map key path wk #'ego-ql--make-dired-action)
      (ego-ql--bind-to-map ego-ql--minibuffer-map key path wk #'ego-ql--make-minibuffer-action)))
  ;; Apply prefix labels
  (dolist (entry (ego-ql--prefix-entries))
    (let ((key   (plist-get entry :key))
          (label (plist-get entry :label)))
      (which-key-add-keymap-based-replacements ego-ql--map key label)
      (which-key-add-keymap-based-replacements ego-ql--minibuffer-map key label))))

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

;;;; Interactive quick-location opener

(defun ego-open-dirvish-at-quick-location ()
  "Open a quick-location directory with completing-read.
Candidates are shown as WK | PATH."
  (interactive)
  (let* ((entries (ego-ql--leaf-entries))
         (candidates
          (mapcar (lambda (e)
                    (let ((wk (ego-ql--entry-wk e))
                          (path (plist-get e :path)))
                      (cons (format "%-12s %s" wk path) path)))
                  entries))
         (choice (completing-read "Quick location: " candidates nil t))
         (path (cdr (assoc choice candidates))))
    (funcall ego-ql--dired-command (expand-file-name path))))

;;;; Project vterm switcher

(defun ego-project-buffers-by-mode (mode)
  "Switch to a buffer with major MODE in the current project.
If only one such buffer exists, switch to it directly.
If several exist, offer completion with buffer preview."
  (if-let* ((proj (project-current))
            (bufs (cl-remove-if-not
                   (lambda (b) (eq (buffer-local-value 'major-mode b) mode))
                   (project-buffers proj)))
            (names (mapcar #'buffer-name bufs)))
      (if (= (length names) 1)
          (switch-to-buffer (car bufs))
        (switch-to-buffer
         (consult--read names
                        :prompt (format "%s: " mode)
                        :category 'buffer
                        :state (consult--buffer-state))))
    (user-error "No %s buffers in this project" mode)))

(provide 'ego)
;;; ego.el ends here
