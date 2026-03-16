;;; setup-local-variables.el --- Machine-specific local variables. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(setenv "PAGER" "less")
(setenv "LESS" "")
(setenv "LESSPIPE" "")

(setq my-local--ego-ql-locations
      '(("d"   . "~/Downloads")
	("h s" . "~/Pictures/screenshots")
	(:key "p" :path "~/Projects" :desc "eql @path")
	(:key "c" :path "~/.config")
	("e r" . "~/.emacs.d/elpaca/repos/")
	("e s" . "/run/user/1000/emacs")
	("R" . "/")
	))


(provide 'setup-local-variables)
;;; setup-local-variables.el ends here
