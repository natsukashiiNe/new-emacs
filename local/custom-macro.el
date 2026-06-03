;;; custom-macro.el --- Macroes I use in my config. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(defmacro l (&rest body)
  "Lamba convinience macro.  Executes BODY."
  `(lambda ()
     ,@body))

(defmacro li (&rest body)
  "Lamba interactive convinience macro.  Executes BODY."
  `(lambda ()
     (interactive)
     ,@body))

(defmacro ow (&rest body)
  "Interactive lambda that runs BODY under the \"other window\" prefix.
Installs `other-window-prefix' so BODY's buffer is displayed in
another window."
  `(lambda ()
     (interactive)
     (other-window-prefix)
     ,@body))

(defmacro fl (&rest body)
  `(funcall (lambda () ,@body)))

(defmacro fli (&rest body)
  `(funcall (lambda () (interactive) ,@body)))

(defmacro iput (expr)
  `(save-excursion
     (end-of-line)
     (insert "\n" (format "%s" ,expr))))

(provide 'custom-macro)
;;; custom-macro.el ends here
