;;; setup-local-variables.el --- Machine-specific local variables. -*- lexical-binding: t; -*-

;;; Commentary:
;; This file probably should not be tracked by git.
;; Only included as an examples.  Possibly move that to README later.

;;; Code:
(setenv "PAGER" "less")
(setenv "LESS" "")
(setenv "LESSPIPE" "")

(setq my-local--ego-ql-locations
      '((:key "h"   :label "home")
	(:key "h s" :path "~/Pictures/screenshots"   :wk "screenshots")
	(:key "p"   :path "~/Projects"               :wk "projects")
	(:key "c"   :path "~/dotfiles/.config"       :wk "config")

        (:key "s"   :label "[s]torage")
	(:key "s v" :path "/storage/vids"            :wk "storage vids")
	(:key "p"   :path "~/Projects"               :wk "projects")
	(:key "c"   :path "~/dotfiles/.config"       :wk "config")

	(:key "e"   :label "Emacs")
	(:key "e r" :path "~/.emacs.d/elpaca/repos/" :wk "elpaca repos")
	(:key "e s" :path "/run/user/1000/emacs"     :wk "sockets")
	(:key "e p" :path "~/.emacs.d/persp-confs"   :wk "perspectives")

	(:key "D"   :path "~/Downloads"              :wk "Downloads")
	(:key "R"   :path "/"                        :wk "root")
	(:key "H"   :path "/home/nane"               :wk "home")
	(:key "T"   :path "/tmp"                     :wk "tmp")))


(provide 'setup-local-variables)
;;; setup-local-variables.el ends here
