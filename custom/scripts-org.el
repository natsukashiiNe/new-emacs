;;; scripts-org.el --- Scripts for org mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Collection of scripts for org mode and note-taking in general.

;;; Code:

(defun my/markdown-to-org ()
  "Convert current markdown file to org format using Pandoc."
  (interactive)
  (let* ((input-file (buffer-file-name))
         (output-file (concat (file-name-sans-extension input-file) ".org")))
    (shell-command
     (format "pandoc %s -f markdown -t org -o %s --wrap=none"
             (shell-quote-argument input-file)
             (shell-quote-argument output-file)))
    (find-file output-file)))

;;; (provide 'scripts-org)
;;; scripts-org.el ends here
