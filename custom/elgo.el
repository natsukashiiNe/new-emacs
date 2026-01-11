;;; elgo.el --- Quick navigation per-project. -*- lexical-binding: t; -*-

;;; Commentary:
;; Define quick-access files per project and map them to keys.

;;; Code:

(defvar-local project-elastic-files nil
  "List of quick-access files for the current project.
Each entry is a plist with :path (relative to project root) and :keymap (single char key).")

(defun my/elastic-files--project-root ()
  "Get the current project root."
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (when-let ((project (project-current)))
        (project-root project))
      default-directory))

(defun my/elastic-files--absolute-path (relative-path)
  "Convert RELATIVE-PATH to absolute path based on project root."
  (expand-file-name relative-path (my/elastic-files--project-root)))

(defun my/elastic-files--display-name (file-plist)
  "Get display name for FILE-PLIST (starting with ./)."
  (concat "./" (plist-get file-plist :path)))

(defun my/elastic-files-open (file-plist)
  "Open file from FILE-PLIST."
  (find-file (my/elastic-files--absolute-path (plist-get file-plist :path))))

(defun my/elastic-files-consult ()
  "Open elastic files with consult preview."
  (interactive)
  (unless project-elastic-files
    (user-error "No elastic files defined for this project"))
  (let* ((candidates
          (mapcar (lambda (file-plist)
                    (let ((abs-path (my/elastic-files--absolute-path
                                     (plist-get file-plist :path))))
                      (cons (my/elastic-files--display-name file-plist)
                            abs-path)))
                  project-elastic-files))
         (selected (consult--read
                    candidates
                    :prompt "Elastic file: "
                    :lookup #'consult--lookup-cdr
                    :state (consult--file-preview)
                    :category 'file
                    :history 'file-name-history
                    :require-match t)))
    (when selected
      (find-file selected))))

(defun my/elastic-files-setup-keys ()
  "Setup keybindings for elastic files based on :keymap property."
  (when project-elastic-files
    (dolist (file-plist project-elastic-files)
      (when-let ((key (plist-get file-plist :keymap))
                 (path (plist-get file-plist :path)))
        ;; Create a named command for better which-key display
        (let ((cmd-name (intern (format "my/elastic-files-open-%s" key)))
              (display-path (concat "./" path)))
          ;; Define the command
          (defalias cmd-name
            `(lambda ()
               (interactive)
               (my/elastic-files-open ',file-plist))
            (format "el-go <%s>" display-path))
          
          ;; Bind the key
          (local-set-key (kbd (concat "M-g " key)) cmd-name)
          
          ;; Register with which-key if available
          (when (fboundp 'which-key-add-key-based-replacements)
            (which-key-add-key-based-replacements
              (concat "M-g " key) (format "elgo <%s>" display-path))))))))

;; Global binding for consult selector
(global-set-key (kbd "M-g M-g") 'my/elastic-files-consult)

;; Register the M-g M-g binding with which-key
(when (fboundp 'which-key-add-key-based-replacements)
  (which-key-add-key-based-replacements
    "M-g M-g" "elastic-files"))

;; Hook to setup keys when opening files
(add-hook 'find-file-hook 'my/elastic-files-setup-keys)


(provide 'elgo)
;;; elgo.el ends here
