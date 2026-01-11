;;; quick-access-files.el --- One-actions to access certain files. -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to access files and projects in one motion.

;;; Code:
(defmacro my/setup-quick-files (&rest files)
  "Define both the config variable and all file commands.
Each element in FILES is a plist with :name, :path, and optional :key."
  `(progn
     (defvar my/quick-open-files ',files
       "Quick-open files configuration.")
     
     ;; Create individual functions for each file
     ,@(mapcar
        (lambda (file-spec)
          (let* ((name (plist-get file-spec :name))
                 (path (plist-get file-spec :path))
                 (func-name (intern (format "my/open-%s" name)))
                 (doc (format "Open %s" path)))
            `(defun ,func-name ()
               ,doc
               (interactive)
               (find-file (expand-file-name ,path)))))
        files)
     
     ;; Create the completion-enabled function
     (defun my/open-files (file-path)
       "Open a quick-access file with completion."
       (interactive
        (list
         (completing-read
          "Open file: "
          (mapcar (lambda (f) (plist-get f :path)) my/quick-open-files)
          nil t)))
       (find-file (expand-file-name file-path)))))


(provide 'quick-access-files)
;;; quick-access-files.el ends here
