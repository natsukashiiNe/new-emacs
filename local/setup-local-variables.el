;;; setup-local-variables.el --- Machine-specific local variables. -*- lexical-binding: t; -*-

;;; Commentary:
;; This file probably should not be tracked by git.
;; Only included as an examples.  Possibly move that to README later.

;;; Code:
(setenv "PAGER" "less")
(setenv "LESS" "")
(setenv "LESSPIPE" "")

(setq my-local--ego-ql-locations
      '(
	(:key "h"   :label "[h]ome")
	(:key "H"   :path "/home/nane"                 :wk "home")
	(:key "h s" :path "~/Pictures/screenshots"     :wk "[s]creenshots")

	(:key "w"     :label "[w]ork dir")
	(:key "W"     :path  "/home/nane/working-dir/"     :wk "[w]ork dir")
	(:key "w o"   :path  "/home/nane/working-dir/org"  :wk "[o]rg notes")
	(:key "w SPC" :path  "/home/nane/working-dir/"     :wk "[w]ork dir")

        (:key "s"     :label "[s]torage")
	(:key "S"     :path  "/storage/"                  :wk "[S]torage")
	(:key "s v"   :path  "/storage/vids"              :wk "[v]ids")
	(:key "s SPC" :path  "/storage/"                  :wk "[S]torage")

	(:key "e"       :label "Emacs")
	(:key "E"       :path  "~/.emacs.d/"                :wk "[E]macs")
	(:key "e r"     :path  "~/.emacs.d/elpaca/sources/" :wk "elpaca repos")
	(:key "e s"     :path  "/run/user/1000/emacs"       :wk "sockets")
	(:key "e p"     :path  "~/.emacs.d/persp-confs"     :wk "perspectives")
	(:key "e SPC"   :path  "~/.emacs.d/"                :wk "[E]macs")

	(:key "P"   :path "~/working-dir/projects/"    :wk "projects")
	(:key "C"   :path "~/dotfiles/.config"         :wk "config")
	(:key "D"   :path "~/Downloads"                :wk "Downloads")
	(:key "R"   :path "/"                          :wk "root")
	(:key "T"   :path "/tmp"                       :wk "tmp")))


(provide 'setup-local-variables)
;;; setup-local-variables.el ends here
