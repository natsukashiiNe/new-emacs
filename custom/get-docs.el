;;; get-docs.el --- Snippets to enhance lisp docs. -*- lexical-binding: t; -*-

;;; Commentary:
;; Snippets to more easier access emacs-lisp's descriptions.

;;; Code:

(defun my/get-variable-docs (pattern)
  "Get documentation for all variables matching PATTERN.
Returns a formatted string with variable name, type, value, and documentation."
  (let ((result nil))
    (mapatoms
     (lambda (sym)
       (when (and (boundp sym)
                  (get sym 'variable-documentation)
                  (string-match-p pattern (symbol-name sym)))
         (push sym result))))
    (setq result (sort result (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
    (mapconcat
     (lambda (sym)
       (let* ((name (symbol-name sym))
              (type (cond
                     ((custom-variable-p sym) "cu")
                     ((local-variable-p sym) "lv")
                     ((boundp sym) "v")
                     (t "?")))
              (value (condition-case nil
                         (let ((val (symbol-value sym)))
                           (cond
                            ((eq val t) "t")
                            ((eq val nil) "nil")
                            ((numberp val) (number-to-string val))
                            ((stringp val) (if (> (length val) 30)
                                               (concat (substring val 0 27) "...")
                                             val))
                            ((keymapp val) "#<keymap>")
                            ((hash-table-p val) "#<hash-table>")
                            ((listp val) (format "(%s ...)" (car-safe val)))
                            (t (format "%S" val))))
                       (error "error")))
              (doc (or (documentation-property sym 'variable-documentation) ""))
              (doc-first-line (if (stringp doc)
                                  (car (split-string doc "[\n\r]+" t " +"))
                                "No documentation.")))
         (format "%s\t\t%s\t\t%s\n\t%s" name type value doc-first-line)))
     result
     "\n")))

(defun my/get-function-docs (pattern)
  "Get documentation for all functions matching PATTERN.
Returns a formatted string with function name, type, value, and documentation."
  (let ((result nil))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-match-p pattern (symbol-name sym)))
         (push sym result))))
    (setq result (sort result (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
    (mapconcat
     (lambda (sym)
       (let* ((name (symbol-name sym))
              (type (cond
                     ((and (fboundp sym) (commandp sym)) "M-x")
                     ((fboundp sym) "fu")
                     (t "?")))
              (value (if (commandp sym) "interactive" "function"))
              (doc (or (documentation sym) ""))
              (doc-first-line (if (stringp doc)
                                  (car (split-string doc "[\n\r]+" t " +"))
                                "No documentation.")))
         (format "%s\t\t%s\t\t%s\n\t%s" name type value doc-first-line)))
     result
     "\n")))

(defun my/get-symbol-docs (pattern)
  "Get documentation for both variables and functions matching PATTERN.
Returns a formatted string with delimiters separating variables and functions."
  (concat "==================== VARIABLES ====================\n"
          (my/get-variable-docs pattern)
          "\n\n==================== FUNCTIONS ====================\n"
          (my/get-function-docs pattern)))

(defun my/show-variable-docs (pattern)
  "Show documentation for all variables matching PATTERN in a new buffer.
Prompts for PATTERN when called interactively."
  (interactive "sVariable pattern: ")
  (let ((output (my/get-variable-docs pattern)))
    (with-current-buffer (generate-new-buffer (format "*Variable Docs: %s*" pattern))
      (insert output)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun my/show-function-docs (pattern)
  "Show documentation for all functions matching PATTERN in a new buffer.
Prompts for PATTERN when called interactively."
  (interactive "sFunction pattern: ")
  (let ((output (my/get-function-docs pattern)))
    (with-current-buffer (generate-new-buffer (format "*Function Docs: %s*" pattern))
      (insert output)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun my/show-symbol-docs (pattern)
  "Show documentation for all symbols (variables and functions) matching PATTERN.
Prompts for PATTERN when called interactively.
Output is displayed in a new buffer with variables and functions separated."
  (interactive "sSymbol pattern: ")
  (let ((output (my/get-symbol-docs pattern)))
    (with-current-buffer (generate-new-buffer (format "*Symbol Docs: %s*" pattern))
      (insert output)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun list-faces-with-hex-colors ()
  "List all faces with their hex color values in a searchable buffer."
  (interactive)
  (let ((faces (face-list))
        (buf (get-buffer-create "*Faces with Hex Colors*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "All Faces with Hex Color Values\n")
      (insert "=================================\n\n")
      (dolist (face (sort faces (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
        (insert (format "\nFace: %s\n" face))
        (dolist (attr '(:foreground :background :weight :slant :underline :overline
				    :strike-through :box :inverse-video :font :height))
          (let* ((val (face-attribute face attr nil 'default))
                 (display-val val))
            (unless (eq val 'unspecified)
              ;; Try to convert color names to hex
              (when (and (stringp val)
                         (memq attr '(:foreground :background))
                         (not (string-prefix-p "#" val)))
                (condition-case nil
                    (let ((rgb (color-name-to-rgb val)))
                      (when rgb
                        (setq display-val
                              (format "%s (#%02x%02x%02x)"
                                      val
                                      (round (* 255 (nth 0 rgb)))
                                      (round (* 255 (nth 1 rgb)))
                                      (round (* 255 (nth 2 rgb)))))))
                  (error nil)))
              (insert (format "  %s: %s\n" attr display-val))))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(provide 'get-docs)
;;; get-docs.el ends here
