;;; custom-macro.el --- Macroes I use in my config. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(defmacro l (&rest body)
  `(lambda ()
     ,@body))

(defmacro li (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(provide 'custom-macro)
;;; custom-macro.el ends here
