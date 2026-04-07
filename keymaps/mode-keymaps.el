;;; mode-keymaps.el --- Mode-specific keymaps. -*- lexical-binding: t; -*-

;;; Commentary:
;; Contains keymaps that are active only in specific modes.

;;; Code:

;; == Global (override) map =====================================================

(defvar my-override-mode-map (make-sparse-keymap)
  "Keymap that overrides all other maps.")

(define-minor-mode my-override-mode
  "Global minor mode for keybindings that must never be shadowed."
  :global t :lighter nil :keymap my-override-mode-map)

(add-to-list 'emulation-mode-map-alists
             `((my-override-mode . ,my-override-mode-map)))

(my-override-mode 1)

;; MAPS
(defvar-keymap my-utility-global-map
  :doc "Global utility map (overwrites everything).")
(keymap-set my-override-mode-map "C-c o" my-utility-global-map)

(defvar-keymap my-agent-shell-map
  :doc "Keys for agent shell actions.")
(keymap-set my-utility-global-map "a" my-agent-shell-map)
(which-key-add-keymap-based-replacements my-utility-global-map
  "a" "agent-shell")

(defvar-keymap my-minuet-map
  :doc "Keys for minuet actions (in-buffer AI auto-completion).")
(keymap-set my-utility-global-map "j" my-minuet-map)
(which-key-add-keymap-based-replacements my-utility-global-map
  "j" "minuet")

(defvar-keymap my-build-normal-map
  :doc "Build map for evil normal state.")
(keymap-set evil-normal-state-map "C-c C-b" my-build-normal-map)

;; TODO: redo in evil-normal / general
(defvar-keymap my-goto-map
  :doc "Map to quickly navigate files (globally).")
(keymap-set global-map "C-c g" my-goto-map)

;; ==== Emacs-(almost)-build-in keymaps ============================================

(keymap-set global-map "C-x C-b" #'projectile-ibuffer)
(keymap-set global-map "C-x B"   #'ibuffer)
(keymap-set global-map "C-x C-x" #'previous-buffer)
(keymap-set global-map "C-x X"   #'next-buffer)
(keymap-set global-map "C-x L"   #'eval-expression)
;; TODO: now held by general
(keymap-set global-map "C-S-l"   #'eval-expression)
(keymap-set global-map "C-c E"   #'eplaca-log)

;; ==== Isearch
(keymap-set global-map "M-l"     #'isearch-forward)
(keymap-set global-map "M-h"     #'isearch-backward)

(keymap-set evil-normal-state-map "n"     #'isearch-repeat-forward)
(keymap-set evil-normal-state-map "N"     #'isearch-repeat-backward)

(keymap-set isearch-mode-map "C-j" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "C-k" #'isearch-repeat-backward)
(keymap-set search-map "h U" #'lazy-highlight-cleanup)

(with-eval-after-load 'consult
  (keymap-set evil-normal-state-map "C-s" 'consult-line)
  (keymap-set evil-visual-state-map "C-s" 'consult-line))

;; ==== Interface
(defvar-keymap my-interface-map
  :doc "My interface keymap"
  "l" #'display-line-numbers-mode)
(keymap-set global-map "C-x i" my-interface-map)

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

(defun my-keymaps-set-activities-global-keymaps ()

  (keymap-set my-goto-map "g" #'persp-switch)
  (keymap-set my-goto-map "i" #'my-persp/switch-to-last-visited)
  (keymap-set my-goto-map "n" #'persp-add-new)
  (keymap-set my-goto-map "s" #'persp-save-state-to-file)
  (keymap-set my-goto-map "l" #'persp-load-state-from-file)

  (keymap-set my-goto-map "C" #'activities-define)
  (keymap-set my-goto-map "o" #'activities-switch)
  (keymap-set my-goto-map "O" #'activities-resume)
  (keymap-set my-goto-map "s" #'activities-suspend)
  (keymap-set my-goto-map "k" #'activities-kill)
  (keymap-set my-goto-map "h" #'activities-switch-buffer)
  (keymap-set my-goto-map "u" #'activities-revert)
  (keymap-set my-goto-map "r" #'activities-rename)
  (keymap-set my-goto-map "l" #'activities-list))

;; == LSP-mode =====================================================
(with-eval-after-load 'lsp-mode
  (evil-define-key '(normal visual) lsp-mode-map
    (kbd "K") nil
    (kbd "J") nil))

;; == COMPILE MAP =====================================================

(defvar-keymap my-compile-map
  :doc "My compile keymap")
(keymap-set global-map "C-c C"  my-compile-map)

(defun define-my-compile-map ()
  "Defines keymaps for the MY-COMPILE-MAP."
  (keymap-set my-compile-map "C" #'my/project-custom-comint)
  (keymap-set my-compile-map "r" #'recompile)
  (keymap-set my-compile-map "o" #'compile)
  (keymap-set my-compile-map "g" #'first-error))

(define-my-compile-map)

;; == QUICK INSERT MAP ================================================

(defvar-keymap my-quick-edit-map
  :doc "Map with to quickly perform often used text-editing actions.")

(keymap-set my-override-mode-map "C-c C-s" my-quick-edit-map)

(defun my-quick-edit-map-setup ()
  "Function to setup quick edit keymaps."

  (keymap-set my-quick-edit-map "h" #'my-edit/copy-previous-word-toggle-case)
  (keymap-set my-quick-edit-map "H" #'my-edit/copy-previous-word)
  (keymap-set my-quick-edit-map "C-h" #'my-edit/avy-copy-word-backward)
  
  (keymap-set my-quick-edit-map "t" (li (transpose-words)))
  (keymap-set my-quick-edit-map "T" (li (transpose-words -1)))

  (keymap-set my-quick-edit-map "W" #'delete-horizontal-space)
  (keymap-set my-quick-edit-map "w" #'delete-all-space)
  (keymap-set my-quick-edit-map "M-w" #'delete-trailing-whitespace)

  (keymap-set my-quick-edit-map "r" #'vr/query-replace)
  (keymap-set my-quick-edit-map "R" #'query-replace)
  
  (keymap-set my-quick-edit-map "S" #'siege-explicit-call)

  (with-eval-after-load 'consult
    (keymap-set my-quick-edit-map "y" #'consult-yank-replace))
  (with-eval-after-load 'yasnippet
    (keymap-set my-quick-edit-map "C-s" #'yas-insert-snippet))
  ;; Comment line insertion
  (with-eval-after-load 'custom-editing
    (keymap-set my-quick-edit-map "c"   #'my/edit-insert-comment-default)
    (keymap-set my-quick-edit-map "C"   #'my/edit-insert-comment)

    ;; (keymap-set my-quick-edit-map "C-t" #'my/edit-insert-comment-default--top)
    ;; (keymap-set my-quick-edit-map "C-b" #'my/edit-insert-comment-default--bottom)
    ;; (keymap-set my-quick-edit-map "M-c" #'my/edit-insert-centered-comment-default)
    ;; (keymap-set my-quick-edit-map "M-t" #'my/edit-insert-centered-comment-default--top)
    ;; (keymap-set my-quick-edit-map "M-b" #'my/edit-insert-centered-comment-default--bottom)
    ;; (keymap-set my-quick-edit-map "M-C" #'my/edit-insert-centered-comment)
    ))
(my-quick-edit-map-setup)

;; == ORG MODE ========================================================

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "C-j") #'org-next-visible-heading
    (kbd "C-k") #'org-previous-visible-heading
    (kbd "C-i") #'org-cycle
    ))

;; !! TO REFACOTR ==
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
    (kbd "M-j") #'flyspell-goto-next-error
    (kbd "M-k") #'flyspell-goto-next-error
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

;; == MINUENT KEYMAPS ===========================================================

(with-eval-after-load 'minuet
  (keymap-set my-utility-global-map "f" #'minuet-complete-with-minibuffer)
  
  (keymap-set my-minuet-map "P" #'minuet-configure-provider))

;; ==== AGENT SHELL KEYMAPS =====================================================

(defun my-keymaps-set-agent-shell-mode ()
  (evil-define-key '(normal visual) agent-shell-mode-map
    (kbd "C-c <return>") #'agent-shell-submit
    (kbd "C-k") #'agent-shell-previous-item
    (kbd "C-j") #'agent-shell-next-item
    (kbd "M-p") #'agent-shell-previous-input
    (kbd "M-n") #'agent-shell-next-input)
  (evil-define-key 'insert agent-shell-mode-map
    (kbd "RET") #'newline
    (kbd "<up>") nil))

(with-eval-after-load 'agent-shell
  (keymap-set my-utility-global-map "o" #'agent-shell-toggle)
  
  (keymap-set my-agent-shell-map    "c"    #'agent-shell-anthropic-start-claude-code)

  (keymap-set my-agent-shell-map "r" #'agent-shell-send-region)
  (keymap-set my-agent-shell-map "f" #'agent-shell-send-file)
  (keymap-set my-agent-shell-map "S" #'agent-shell-send-screenshot))


(provide 'mode-keymaps)
;;; mode-keymaps.el ends here
