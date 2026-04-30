;;; mode-keymaps.el --- Mode-specific keymaps. -*- lexical-binding: t; -*-

;;; Commentary:
;; Contains keymaps that are active only in specific modes.

;;; Code:
(require 'keymap-utils)

;; == Global (override) map =====================================================

(defvar my-override-mode-map (make-sparse-keymap)
  "Keymap that overrides all other maps.")

(define-minor-mode my-override-mode
  "Global minor mode for keybindings that must never be shadowed."
  :global t :lighter nil :keymap my-override-mode-map)

(add-to-list 'emulation-mode-map-alists
             `((my-override-mode . ,my-override-mode-map)))

(my-override-mode 1)

;; == MAPS ======================================================================

(my-keymaps/define-submaps global-map
  (my-maps/compile-map    :prefix "C-c C"    :doc "My compilation map." :wk "[C]ompile")
  (my-maps/build-map      :prefix "C-c B"    :doc "My build map."       :wk "[B]uild")
  (my-maps/diff-map       :prefix "C-c C-d"  :doc "My dif map."         :wk "[D]iff"))

(my-keymaps/define-submaps my-override-mode-map
  (my-maps/global-utility   :prefix "C-c o"   :doc "Global utility map." :wk "Utility")
  (my-maps/quick-edit       :prefix "C-c C-s" :doc "Quick editing."      :wk "Quick-edit")
  (my-maps/quick-open       :prefix "M-g"     :doc "Quick access map."   :wk "Quick-locations")
  :populate
  ("M-h"  #'isearch-backward  "isearch-backward")
  ("M-l"  #'isearch-forward   "isearch-forward"))

(my-keymaps/define-submaps my-maps/global-utility
  (my-maps/agent-shell :prefix "a" :doc "Agent shell map."                  :wk "AI [a]gent")
  (my-maps/minuet      :prefix "j" :doc "Minuet (AI-power auto-completion)" :wk "minuet")
  :populate-after 'minuet
  ("f" #'minuet-complete-with-minibuffer "AI auto-completion")
  :populate-after 'agent-shell
  ("o" #'agent-shell-toggle))

;; == Emacs-(almost)-build-in keymaps  ==========================================

(keymap-set global-map "C-x C-b" #'projectile-ibuffer)
(keymap-set global-map "C-x B"   #'ibuffer)
(keymap-set global-map "C-x C-x" #'previous-buffer)
(keymap-set global-map "C-x X"   #'next-buffer)
(keymap-set global-map "C-x L"   #'eval-expression)
;; TODO: now held by general
(keymap-set global-map "C-S-l"   #'eval-expression)
(keymap-set global-map "C-c E"   #'eplaca-log)

;; ---- Isearch -----------------------------------------------------------------

(keymap-set global-map "M-l"     #'isearch-forward)
(keymap-set global-map "M-h"     #'isearch-backward)

;; n/N uses isearch
(keymap-set evil-normal-state-map "n"
	    (li (isearch-repeat-forward) (isearch-lazy-highlight-new-loop)))
(keymap-set evil-normal-state-map "N"
	    (li (isearch-repeat-backward) (isearch-lazy-highlight-new-loop)))
(keymap-set search-map "h U" (li (isearch-dehighlight) (lazy-highlight-cleanup t)))

(keymap-set isearch-mode-map "C-j" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "C-k" #'isearch-repeat-backward)
(keymap-set isearch-mode-map "C-S-V" #'isearch-yank-kill)

(with-eval-after-load 'consult
  (keymap-set evil-normal-state-map "C-s" 'consult-line)
  (keymap-set evil-visual-state-map "C-s" 'consult-line))

(with-eval-after-load 'treesit-jump
  (keymap-set global-map "C-c C-t" #'treesit-jump-transient))


;; ---- Evil-insert -------------------------------------------------------------
(keymap-set evil-insert-state-map "C-a" #'move-beginning-of-line)
(keymap-set evil-insert-state-map "C-e" #'move-end-of-line)

;; ---- Interface ---------------------------------------------------------------
;; TODO  define submap + poplute
(defvar-keymap my-interface-map
  :doc "My interface keymap"
  "l" #'display-line-numbers-mode
  "L" #'hl-line-mode
  "m" #'hide-mode-line-mode

  "f t" #'toggle-frame-tab-bar
  )
(keymap-set global-map "C-x i" my-interface-map)
(which-key-add-keymap-based-replacements my-interface-map
  "f" "frame settigns"
  "H" "hook toggles")

(with-eval-after-load 'flymake-diagnostics
  (keymap-set my-interface-map "H f" #'my/evil-toggle-enter-insert-flymake-hook)
  (keymap-set my-interface-map "H F" #'my/evil-toggle-exit-insert-flymake-hook))

(with-eval-after-load 'posframe-settings
  (keymap-set my-interface-map "H p" #'my/toggle-posframe-minibuffer-hooks))


(keymap-set minibuffer-local-map "C-S-v" 'evil-paste-before)
(keymap-set minibuffer-local-map "<escape>" 'abort-recursive-edit)
(keymap-set minibuffer-local-map "C-c o" (li
					  (my/set-vertico-count
					   (+ vertico-count 10))))

(keymap-set minibuffer-local-map
            "C-c w"
            (lambda ()
              (interactive)
              (my/set-vertico-count
               (max 1 (- vertico-count 10)))))


;; == Evil-Normal =====================================================
(with-eval-after-load 'evil
  (keymap-set evil-normal-state-map "J" #'next-error)
  (keymap-set evil-normal-state-map "K" #'previous-error)
  (keymap-set evil-normal-state-map "s" #'vr/isearch-backward)
  ;;(keymap-set evil-normal-state-map "M-f" #'vr/isearch-backward)
  )

(keymap-set isearch-mode-map "C-c"
	    (li (isearch-exit) (message "hello!" ) (avy-isearch)))

(evil-define-key '(normal visual motion) prog-mode-map
  (kbd "g c") #'comment-line)

(with-eval-after-load 'symbol-overlay
  (evil-define-key 'normal 'symbol-overlay-mode-map
    (kbd "M-n") (li (symbol-overlay-jump-next) (recenter))
    (kbd "M-p") (li (symbol-overlay-jump-prev) (recenter))))

;; == COMPILE MAP =====================================================

(my-keymaps/populate my-maps/compile-map
  ("C"   #'my/project-custom-comint "custom-comint")
  ("r"   #'recompile                "recompile")
  ("o"   #'compile)
  ("g"   #'first-error              "first error"))

;; == QUICK LOCATIONS ===========================================================

(my-keymaps/populate my-maps/quick-open
  ("C-o" (li (my/project-switch-projectile 'dired)))
  ("M-d" #'devdocs-lookup)
  :after persp-mode
  ("M-s" #'persp-switch)
  ("M-e" #'my-persp/switch-to-last-visited)
  :after 'ego
  ("M-w"   (li (ego-project-buffers-by-mode 'vterm-mode)) "project vterms"))


;; == Quick INSERT MAP ================================================

(my-keymaps/populate my-maps/quick-edit
  ("t"   (li (transpose-words 1))                 "transpose >")
  ("T"   (li (transpose-words -1))                "transpose <")
  ("W"   #'delete-horizontal-space)
  ("w"   #'delete-all-space)
  ("M-w" #'delete-trailing-whitespace)
  ("R"   #'query-replace             "query replace")
  :after 'siege
  ("S"   #'siege-explicit-call)
  :after 'visual-regexp
  ("r"   #'vr/query-replace          "query replace (rx)")
  :after 'custom-editing
  ("h"   #'my-edit/copy-previous-word-toggle-case "copy prev word toggle")
  ("H"   #'my-edit/copy-previous-word             "copy prev word")
  ("C-h" #'my-edit/avy-copy-word-backward         "copy prev word avy")
  :after 'consult
  ("y"   #'consult-yank-replace      "yank replace")
  :after 'yasnippet
  ("C-s" #'yas-insert-snippet        "snippet")
  :after 'custom-editing
  ("c"   #'my/edit-insert-comment-default "comment")
  ("C"   #'my/edit-insert-comment         "comment (prompt)"))


;; == MINUET ====================================================================

(with-eval-after-load 'minuet
  (my-keymaps/populate my-maps/minuet
    ("P" #'minuet-configure-provider)))

;; ==== AGENT SHELL KEYMAPS =====================================================

(with-eval-after-load 'agent-shell
  ;; global map
  (my-keymaps/populate my-maps/agent-shell
    ("M-c"    #'agent-shell-anthropic-start-claude-code)
    ("r"    #'agent-shell-send-region)
    ("f"    #'agent-shell-send-file)
    ("S"    #'agent-shell-send-screenshot))
  ;; AGENT-SHELL-MODE
  (evil-define-key '(normal visual) agent-shell-mode-map
    (kbd "C-c RET") #'agent-shell-submit
    (kbd "C-k") #'agent-shell-previous-item
    (kbd "C-j") #'agent-shell-next-item
    (kbd "M-p") #'agent-shell-previous-input
    (kbd "M-n") #'agent-shell-next-input)
  (evil-define-key 'insert agent-shell-mode-map
    (kbd "RET") #'newline
    (kbd "<up>") nil))


;; == Lisp Editing ==============================================================
(defvar-keymap my-lisp-edit-map
  :doc "Map to edit lisp expressions")
(keymap-set global-map "C-c i" my-lisp-edit-map)

(defvar-keymap my-lsp-map
  :doc "Map to edit lisp expressions")
(keymap-set global-map "C-c i" my-lsp-map)

(with-eval-after-load 'puni
  (keymap-set my-lisp-edit-map "o" #'my/lisp-edit-append-end-of-sexp)
  (keymap-set my-lisp-edit-map "i" #'my/lisp-edit-insert-end-of-sexp))

(defun my/keymaps-set-sly-mode()
  (evil-define-key 'insert sly-mode-map
    (kbd "RET") #'newline
    (kbd "RET") #'newline
    )
  (evil-define-key 'normal sly-mode-map
    (kbd "RET") #'sly-mrepl-return))

(my/keymaps-set-sly-mode)

;; == PROJECTILE ======================================================

;; == DIRVVISH ========================================================
(defun my-keymaps-set-dired-mode ()
  (evil-define-key 'normal dired-mode-map
    (kbd "T") #'dired-create-empty-file
    (kbd "o") (li (other-window-prefix)
		  (find-file (dirvish-prop :index))) ;; open in a different window
    (kbd "TAB")  #'dirvish-toggle-preview     ; Toggle preview on/off
    (kbd "SPC")  #'dirvish-show-history
    (kbd "b"  )  #'dirvish-goto-bookmark
    (kbd "z"  )  #'dirvish-history-jump
    (kbd "f"  )  #'dirvish-fd-jump            ; Use fd to jump to file
    (kbd "s"  )  #'dirvish-quicksort
    (kbd "y"  )  #'dirvish-yank
    (kbd "h"  )  #'dired-up-directory
    (kbd "l"  )  #'dired-find-file
    (kbd "a"  )  #'dirvish-quick-access
    (kbd "q"  )  #'dirvish-quit
    (kbd "C-g")  #'dirvish-quit))

(my-keymaps-set-dired-mode)

;; == GOTO ============================================================

;; (defun my-keymaps-set-activities-global-keymaps ()

;;   (keymap-set my-activities-map "g" #'persp-switch)
;;   (keymap-set my-activities-map "i" #'my-persp/switch-to-last-visited)
;;   (keymap-set my-activities-map "n" #'persp-add-new)
;;   (keymap-set my-activities-map "s" #'persp-save-state-to-file)
;;   (keymap-set my-activities-map "l" #'persp-load-state-from-file)

;;   (keymap-set my-activities-map "C" #'activities-define)
;;   (keymap-set my-activities-map "o" #'activities-switch)
;;   (keymap-set my-activities-map "O" #'activities-resume)
;;   (keymap-set my-activities-map "s" #'activities-suspend)
;;   (keymap-set my-activities-map "k" #'activities-kill)
;;   (keymap-set my-activities-map "h" #'activities-switch-buffer)
;;   (keymap-set my-activities-map "u" #'activities-revert)
;;   (keymap-set my-activities-map "r" #'activities-rename)
;;   (keymap-set my-activities-map "l" #'activities-list))

;; == LSP-mode =====================================================
(with-eval-after-load 'lsp-mode
  (evil-define-key '(normal visual) lsp-mode-map
    (kbd "K") nil
    (kbd "J") nil))


;; == ORG MODE ========================================================

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "C-j") #'org-next-visible-heading
    (kbd "C-k") #'org-previous-visible-heading
    (kbd "C-i") #'org-cycle
    ))

;; !! TODO REFACOTR ==
;;(keymap-set evil-insert-state-map "C-h" 'evil-delete-backward-char)
;; (keymap-set evil-normal-state-map "C-x C-h" 'consult-org-heading)

(with-eval-after-load 'xref
  (evil-define-key 'normal xref--xref-buffer-mode-map
    (kbd "S-j") #'xref-next-line
    (kbd "S-k") #'xref-prev-line))

;; vdiff
(with-eval-after-load 'vdiff
  (evil-define-key 'normal vdiff-mode-map
    (kbd "S-j") #'vdiff-next-hunk
    (kbd "S-k") #'vdiff-previous-hunk))

(with-eval-after-load 'magit
  ;; C-x 1..4 section-level bindings across all magit modes
  (dolist (map (list magit-status-mode-map
                     magit-log-mode-map
                     magit-stash-mode-map
                     magit-revision-mode-map
                     magit-diff-mode-map))
    (evil-define-key 'normal map
      (kbd "C-x 1") #'magit-section-show-level-1-all
      (kbd "C-x 2") #'magit-section-show-level-2-all
      (kbd "C-x 3") #'magit-section-show-level-3-all
      (kbd "C-x 4") #'magit-section-show-level-4-all))

  ;; magit-status extra binding
  (evil-define-key 'normal magit-status-mode-map
    (kbd "M-s M-s") #'magit-stage)

  ;; vdiff bindings
  (dolist (map (list magit-stash-mode-map magit-revision-mode-map))
    (evil-define-key 'normal map
      (kbd "e") #'vdiff-magit-dwim
      (kbd "E") #'vdiff-magit))
  (evil-define-key 'normal magit-commit-section-map
    (kbd "s") #'vdiff-magit-dwim))

(with-eval-after-load 'embark
  (evil-define-key 'normal embark-collect-mode-map
    (kbd "K") #'outline-previous-heading
    (kbd "J") #'outline-next-heading
    (kbd "H") #'outline-hide-body
    (kbd "L") #'outline-show-all))

(with-eval-after-load 'flymake
  (evil-define-key 'normal flymake-mode-map
    (kbd "C-f C-d") #'consult-flymake
    (kbd "C-j") #'flymake-goto-next-error
    (kbd "C-k") #'flymake-goto-prev-error
    ;; TODO: move to appropriate mode setup.
    (kbd "M-j") #'compilation-next-error
    (kbd "M-k") #'compilation-previous-error))

(with-eval-after-load 'flyspell
  (evil-define-key 'normal flyspell-mode-map
    (kbd "C-f C-d") #'consult-flyspell
    (kbd "M-j") #'evil-next-flyspell-error
    (kbd "M-k") #'evil-prev-flyspell-error
    ))

(with-eval-after-load 'lsp-ui
  (evil-define-key 'normal lsp-ui-mode-map
    (kbd "C-c d d") #'lsp-ui-doc-glance
    ))

(with-eval-after-load 'flyspell
  (evil-define-key 'normal org-mode-map
    (kbd "C-f C-w") #'consult-flyspell))

(with-eval-after-load 'evil
  (evil-define-key 'normal org-mode-map
    (kbd "C-f i") #'consult-org-heading))

(with-eval-after-load 'devdocs
  (evil-define-key 'normal devdocs-mode-map
    (kbd "n") #'devdocs-go-forward
    (kbd "p") #'devdocs-go-back))

					; == GLOBAL UTILITY ============================================================

(my-keymaps/populate my-maps/global-utility
  )

;; == MINUENT KEYMAPS ===========================================================





(provide 'mode-keymaps)
;;; mode-keymaps.el ends here
