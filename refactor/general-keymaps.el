;;; general-keymaps.el --- General (global overrides) keymaps. -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up General keymaps that would be avalable in any mode.

;;; Code:

;;; TODO
;; Create 3 files of that:
;; - mode-keympas -> which would define functions to define kyes on a specific modeso
;; general-keymaps -> which would define global keys
;; scrpts/* -> all the scripts

(define-key input-decode-map "\e[104;7u" (kbd "C-M-h"))
(define-key input-decode-map "\e[106;7u" (kbd "C-M-j"))
(define-key input-decode-map "\e[107;7u" (kbd "C-M-k"))
(define-key input-decode-map "\e[108;7u" (kbd "C-M-l"))

(defun my/exec-with-prefix (prefix)
  "Execute `execute-extended-command' with PREFIX pre-inserted in minibuffer."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert prefix))
    (command-execute #'execute-extended-command)))

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

;; Define your quick-access files
(my/setup-quick-files
 (:name "nvidia-drm"
        :key "n d"
        :path "/usr/share/X11/xorg.conf.d/10-nvidia-drm-outputclass.conf")
 
 (:name "nvidia-conf"
        :key "n c"
        :path "/etc/modprobe.d/nvidia.conf")
 
 ;; Add more *files as needed
 (:name "xorg-conf"
        :key "x"
        :path "/etc/X11/xorg.conf")

 (:name "xinitrc"
        :key "X"
        :path "/home/nane/.config/X11/.xinitrc")

 (:name "clangd-format"
        :key "c f"
        :path "/home/nane/.clang-format")
 )



(defun my/persp-open-project (persp-name project-root &optional mode)
  "Switch Perspective and Projectile with different opening modes.
Switch to PERSP-NAME (creating it if needed), switch Projectile to PROJECT-ROOT
and open it with MODE.

MODE can be:
  'magit  - Open magit-status (default)
  'Dired  - Open Dired in project root
  'find   - Open projectile-find-file prompt
  'shell  - Open shell in project root
  'eshell - Open eshell in project root
  nil     - Default to magit"
  (interactive "sPerspective name: \nDProject root: ")
  (persp-mode 1)
  (let* ((pr (file-truename (expand-file-name project-root)))
         (mode (or mode 'magit)))
    (unless (file-directory-p pr)
      (user-error "Not a directory: %s" pr))
    
    ;; Switch to perspective first, so buffers/windows land there
    (persp-switch persp-name)
    
    ;; Make sure Projectile knows about the project
    (projectile-add-known-project pr)
    
    ;; Override the default action based on mode
    (let ((projectile-switch-project-action
           (pcase mode
             ('magit  (lambda () (magit-status pr)))
             ('dired  (lambda () (dired pr)))
             ('find   (lambda () (projectile-find-file)))
             ('shell  (lambda () (projectile-run-shell)))
             ('eshell (lambda () (projectile-run-eshell)))
             (_       (lambda () (magit-status pr))))))
      ;; Jump straight to the project and run the action
      (projectile-switch-project-by-name pr)))
  )

(defmacro my/setup-quick-projects (&rest projects)
  "Define both the config variable and all project commands.
Each element in PROJECTS is a plist with :name, :persp, :path, and optional :mode."
  `(progn
     (defvar my/quick-open-projects ',projects
       "Quick-open projects configuration.")
     ,@(mapcar
        (lambda (proj)
          (let* ((name (plist-get proj :name))
                 (persp (plist-get proj :persp))
                 (path (plist-get proj :path))
                 (mode (or (plist-get proj :mode) 'magit))
                 (func-name (intern (format "my/open-%s" name)))
                 (mode-desc (pcase mode
                              ('magit "magit")
                              ('dired "dired")
                              ('find "find-file")
                              ('shell "shell")
                              ('eshell "eshell")
                              (_ "default")))
                 (doc (format "Open %s in a new perspective (%s)." persp mode-desc)))
            `(defun ,func-name ()
               ,doc
               (interactive)
               (my/persp-open-project ,persp ,path ',mode))))
        projects)))

(my/setup-quick-projects

 ;; Configs (dotfiles
 (:name dotfiles
        :mode magit
        :persp "Dotfiles"
        :path "~/dotfiles")
 (:name emacs-config
        :mode magit
        :persp "Emacs-config"
        :path "~/.config/emacs")
 (:name nvim-config
        :mode magit
        :persp "nvim-config"
        :path "~/.config/nvim")

 ;; Projects
 (:name herb
        :mode magit
        :persp "Herb"
        :path "~/_projects/clones/herbstluftwm")
 (:name vasiniyo
        :mode magit
        :persp "Vasiniyo"
        :path "~/_projects/clones/vasiniyo-chat-bot")
 (:name piechat
        :mode dired
        :persp "Piechat"
        :path "~/_projects/piechat")
 (:name pyssr
        :mode dired
        :persp "Piechat"
        :path "~/.config/herbstluftwm/pyssr")

 ;; notes / chats
 (:name lisp-notes
        :mode dired
        :persp "Lisp-notes"
        :path "~/notes/learn/lisp")
 (:name notes
        :mode dired
        :persp "notes"
        :path "~/notes")
 (:name books
        :mode dired
        :persp "books"
        :path "~/_books")
 )


(use-package general
  :ensure t
  :after evil

  :config
  ;; reapply evil-mode (needs to properly hook general)
  (message "Applying general keybinds!")

  ;; UNPREFIXED
  (general-define-key
   :states '(normal visual insert)
   :keymaps 'override
   "M-l" 'tab-bar-switch-to-next-tab
   "M-h" 'tab-bar-switch-to-prev-tab
   "M-i" 'evil-switch-to-windows-last-buffer
   "M-e" 'eyebrowse-last-window-config

   "C-S-H" 'evil-window-left
   "C-S-J" 'evil-window-down
   "C-S-K" 'evil-window-up
   "C-S-L" 'evil-window-right

   "C-M-h" 'shrink-window-horizontally
   "C-M-l" 'enlarge-window-horizontally
   "C-M-j" 'enlarge-window
   "C-M-k" 'shrink-window

   "M-t" 'elastic-vterm-toggle)

  (general-define-key
   :states  '(normal motion)
   :keymaps 'override
   ;; "s" #'avy-goto-char-2
   )

  ;; Consult-commands
  (general-create-definer my-jump-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "C-f")
  (my-jump-leader
    ;; projects
    "C-e"     '(persp-switch-last :which-key "persp last")
    "j"       '(eyebrowse-switch-to-window-config :which-key "eye switch")
    "O"       '(projectile-switch-project :which-key "project [o]pen")
    "C-w"     '(persp-switch      :which-key "persp switch")
    ;; Specific projectiles:
    "o"       '(:ignore t :which-key "[o]pen file")
    "o j"     '(:ignore t :which-key "open pro[j]ect")
    "o d"     '(:ignore t :which-key "open [d]otfile")
    "o o"     '(:ignore t :which-key "open n[o]tes/[o]rg")

    "o d d"     '(my/open-dotfiles       :which-key "[D]otfiles (persp+magit)")
    "o d e"     '(my/open-emacs-config   :which-key "[E]macs (persp+magit)")
    "o d n"     '(my/open-nvim-config    :which-key "[N]nvim (persp+magit)")
    "o o l"     '(my/open-lisp-notes     :which-key "[L]isp-notes (persp+dired)")
    "o o o"     '(my/open-notes          :which-key "[N]otes (persp+dired)")
    "o o b"     '(my/open-books          :which-key "[B]otes (persp+dired)")
    "o j c"     '(my/open-piechat        :which-key "[P]iechat (persp+dired)")
    "o j h"     '(my/open-herb           :which-key "[H]erb (persp+magit)")
    "o j v"     '(my/open-vasiniyo       :which-key "[V]asiniyo (persp+magit)")

    ;; FILES
    "o F"       '(my/open-files :which-key "open [F]ile (quick)")
    ;; Specific file mappings
    "o f"       '(:ignore t :which-key "open [f]ile (direct)")
    "o f n"     '(:ignore t :which-key "[n]vidia configs")
    "o f n d"   '(my/open-nvidia-drm :which-key "nvidia [d]rm output")
    "o f n c"   '(my/open-nvidia-conf :which-key "nvidia [c]onf")
    "o f x"     '(my/open-xorg-conf :which-key "[x]org.conf")

    ;; consult multi-files
    "F"       '(my-consult-projectile-find-file   :which-key "[f]ind file")
    "C-f"     '(consult-project-buffer :which-key "buffers")
    "G"       '(consult-ripgrep        :which-key "r[g]")
    "S"       '(consult-lsp-symbols    :which-key "lsp [S]ymbols")

    "t"       '(:ignore t :which-key "[t]est t")
    "t f"     '(consult-projectile-find-file   :which-key "[f]ind file")
    "t p"     '(consult-projectile   :which-key "[f]ind file")

    "I"     '(consult-info             :which-key "[i]nfo")
    "D"     '(devdocs-lookup           :which-key "[d]ocs lookup")

    "E"     '(consult-compile-error    :which-key "compilation [E]rror")
    ;; TODO: pre-complited infos
    ;; TODO: docs

    ;; consult in-buffer
    "i"   '(consult-imenu               :which-key "[i]menu")
    "l"   '(consult-outline             :which-key "out[l]ine")
    "J"   '(evil-collection-consult-jump-list :which-key "evil [j]ump list")
    "m"   '(evil-collection-consult-mark      :which-key "evil [m]arks")
    "C-s" '(consult-lsp-file-symbols          :which-key "lsp file [s]yms")
    )

  ;; SPC prefix
  (general-create-definer my-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (my-leader
    ;; basics
    "i" '(next-buffer     :which-key "next buffer")
    "o" '(previous-buffer :which-key "prev buffer")
    "I" '(winner-redo     :which-key "winner next")
    "O" '(winner-undo     :which-key "winner prev")

    ;; perspectives
    "C-j"         '(:ignore t :which-key "[+] persp-switch")
    "C-j C-j"     '(persp-switch :which-key "[+] persp-switch")
    "C-j i"       '(tab-bar-switch-to-recent-tab   :which-key "recent tab")
    "C-j e"       '(projectile-previous-project-buffer   :which-key "recent buffer")
    "C-j C-s"     '(persp-save   :which-key "save perspective")
    "C-j C-e"     '(persp-switch-last :which-key "switch last")
    ;; TODO eyebrowse
    "C-j C-h"     '(:ignore t :which-key "projectile managment")
    "C-j C-h C-h" '(projectile-switch-project :which-key "projectle switch project")
    ;; Specific projectiles:
    "C-j C-h D"   '(my/open-dotfiles :which-key "[D]otfiles (persp+magit)")
    "C-j C-h H"   '(my/open-herb :which-key "[H]erb (persp+magit)")
    "C-j C-h V"   '(my/open-vasiniyo :which-key "[V]asiniyo (persp+magit)")
    
    ;; “SPC f” prefix for “search” or “files”
    "f"   '(:ignore t :which-key "[+] search / files")
    "f f" '(projectile-find-file :which-key "projectile-find-file")
    "f j" '(consult-project-buffer :which-key "consult-project-buffer")
    "f J" '(pop-to-buffer :which-key "pop-to-buffer")
    "f g" '(consult-ripgrep :which-key "consult-r[g]")
    "f G" '(projectile-ripgrep :which-key "projectile r[g]")
    "f s" '(consult-lsp-file-symbols :which-key "lsp symbols")
    "f S" '(consult-lsp-symbols :which-key "lsp symbols")
    "f e" '(projectile-dired :which-key "dired in project")


    ;; Buffer
    "y"   '(avy-copy-line :which-key "avy-copy-line")

    ;; “SPC t” tab managment
    "t"   '(:ignore t :which-key "tabs managment")
    "t n"   '(tab-new-to :which-key "new tab")
    "t c"   '(tab-close :which-key "close tab")
    "t t"   '(other-tab-prefix :which-key "other-tab-prefix")

    ;; "SPC g" for git
    "g"   '(:ignore t :which-key "git managment")
    "g g"   '(magit :which-key "magit")

    ;; “SPC h” prefix for help or “completions”
    "h"   '(:ignore t :which-key "[+] help / completions")
    "h e" '((lambda () (interactive) (my/exec-with-prefix "evil- "))
            :which-key "[e]vil commands")
    "h C" '((lambda () (interactive) (my/exec-with-prefix "evilnc- "))
            :which-key "evil [c]omment commands")
    "h h" '((lambda () (interactive) (my/exec-with-prefix "describe- "))
            :which-key "describe")
    "h c" '((lambda () (interactive) (my/exec-with-prefix "consult- "))
            :which-key "[c]onsult commands")
    "h p" '((lambda () (interactive) (my/exec-with-prefix "projectile- "))
            :which-key "[p]rojectile commands")
    "h b" '((lambda () (interactive) (my/exec-with-prefix "eyebrowse- "))
            :which-key "eye[b]rowse commands")
    "h d" '((lambda () (interactive) (my/exec-with-prefix "dired - "))
            :which-key "[d]ired commands")
    "h l" '((lambda () (interactive) (my/exec-with-prefix "lsp - "))
            :which-key "[l]sp commands")
    "h t" '((lambda () (interactive) (my/exec-with-prefix "treemacs - "))
            :which-key "treemacs commands")
    "h n" '((lambda () (interactive) (my/exec-with-prefix "persp - "))
            :which-key "persp commands")
    "h g" '(:ignore t :which-key "[+] ma[g]it")
    "h g d" '((lambda () (interactive) (my/exec-with-prefix "vdiff-magit- "))
              :which-key "v[d]iff")

    ;; “SPC j” for LSP multi-file navigation
    "j"   '(:ignore t :which-key "[+] lsp")
    "j h" '(lsp-clangd-find-other-file        :which-key "switch .[h]pp/.cpp")
    "j l" '(lsp-find-references               :which-key "find references")
    "j r" '(lsp-rename                        :which-key "smart [r]ename")
    "j s" '(lsp-signature                     :which-key "lsp [s]ignature")
    "j d" '(lsp-find-definition               :which-key "goto definition")
    "j i" '(lsp-find-implementation           :which-key "goto [i]mpl.")
    "j w" '(lsp-ui-doc-toggle                 :which-key "toggle doc")
    "j t" '(lsp-find-type-definition          :which-key "[t]ype definition")

    ;; “SPC b” for buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "b e" '(eval-buffer :which-key "evaluate buffer")

    ;; “SPC S” for system ops
    "S"   '(:ignore t :which-key "system")
    "S f" '(list-faces-display :which-key "list faces")

    ;; interface
    "u e" '(treemacs-select-window :which-key "treemacs")
    "u f" '(flycheck-list-errors :which-key "flycheck-list-errors")

    ))

;; =============================== EVIL NORMAL KEYMAPS ===============================

;; ============================== MODE SPECIFIC KEYMAPS ==============================
;; (require dash)
;; (defmacro define-evil-keys-for-maps (state maps &rest bindings)
;;   "Define BINDINGS for STATE in multiple MAPS.
;; MAPS is a list of map symbols.
;; BINDINGS are pairs of (key . command)."
;;   =(dolist (map-sym ',maps)
;;      (evil-define-key ',state (symbol-value map-sym)
;;        ,@(cl-loop for (key . cmd) in bindings
;;                   append (list =(kbd ,key) =#',cmd)))))



;; Lsp-based buffers
;; (defvar my-lsp-leader-map
;;   (define-keymap
;;     "h" #'lsp-clangd-find-other-file
;;     "l" #'lsp-find-references
;;     "r" #'lsp-rename
;;     "s" #'lsp-signature
;;     "d" #'lsp-find-definition
;;     "m" #'lsp-find-implementation
;;     "w" #'lsp-ui-doc-toggle
;;
;;     "c" #'consult-flymake
;;     "p" #'flymake-show-project-diagnostics
;;     "j" #'flymake-goto-next-error
;;     "k" #'flymake-goto-prev-error)
;;   "Leader keymap for LSP")
;; 
;; ;; Bind SPC j globally in evil normal state
;; (keymap-set evil-normal-state-map "SPC j" my-lsp-leader-map)

;; ORG MODE KEYMAPS
(with-eval-after-load 'org
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix "C-f"
   "r d" 'org-remark-mark-definition
   "r f" 'my/consult-definitions
   "r n" 'org-remark-next
   "r p" 'org-remark-prev
   "r r" 'org-remark-remove
   "C-r" 'my/consult-definitions
   ))




;; evil insert

;; VTERM
;;(keymap-set  vterm-mode "C-M-l" 'tab-bar-switch-to-next-tab)
;;(keymap-set  vterm-mode "M-C-h" 'tab-bar-switch-to-prev-tab)



;; (with-eval-after-load 'evil
;;   (define-key minibuffer-local-map (kbd "C-h") #'evil-delete-char)
;;   (define-key minibuffer-local-completion-map (kbd "C-h") #'evil-delete-char)
;;   (define-key minibuffer-local-map (kbd "C-w") #'backward-kill-word)
;;   (define-key minibuffer-local-completion-map (kbd "C-w") #'backward-kill-word)
;;   ;; If needed, also:
;;   ;; (define-key minibuffer-local-ns-map (kbd "C-h") #'backward-kill-word)
;;   ;; (define-key minibuffer-local-isearch-map (kbd "C-h") #'backward-kill-word)
;;   )
;; 
;; (evil-define-key 'insert evil-insert-state-map
;;   (kbd "C-h") #'evil-delete-backward-char)




(provide 'general-keymaps)
;;; general-keymaps.el ends here
