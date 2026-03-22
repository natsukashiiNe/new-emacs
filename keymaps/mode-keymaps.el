;;; mode-keymaps.el --- Mode-specific keymaps. -*- lexical-binding: t; -*-

;;; Commentary:
;; Contains keymaps that are active only in specific modes.

;;; Code:
;; MAPS
(defvar-keymap my-utility-normal-map
  :doc "Utility map for evil normal state (agent-shell).")
(keymap-set evil-normal-state-map "M-l" my-utility-normal-map)

(defvar-keymap my-utility-insert-map
  :doc "Utility map for evil insert state (agent-shell).")
(keymap-set evil-insert-state-map "M-l" my-utility-insert-map)

(defvar-keymap my-build-normal-map
  :doc "Build map for evil normal state.")
(keymap-set evil-normal-state-map "C-c C-b" my-build-normal-map)

(defvar-keymap my-quick-edit-map
  :doc "Map with to quickly insert often used text.")
(keymap-set global-map "C-c C-s" my-quick-edit-map)

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

(keymap-set isearch-mode-map "C-c" (li (isearch-exit) (message "hello!" ) (avy-isearch)))

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
(defun my-keymaps-set-lsp-global-keymaps ()
  (with-eval-after-load 'evil
    (evil-define-key '(normal visual)
      (kbd "K") nil
      (kbd "J") nil)
    ))

;; == COMPILE MAP =====================================================

(defvar-keymap my-compile-map
  :doc "My compile keymap")
(keymap-set global-map "C-c C-c"  my-compile-map)

(defun define-my-compile-map ()
  "Defines keymaps for the MY-COMPILE-MAP."
  (keymap-set my-compile-map "C" #'my/project-custom-comint)
  (keymap-set my-compile-map "r" #'recompile)
  (keymap-set my-compile-map "o" #'compile)
  (keymap-set my-compile-map "g" #'first-error))

(define-my-compile-map)


;; == QUICK INSERT MAP ================================================
(defun my-quick-edit-map-setup ()
  (keymap-set my-quick-edit-map "t" (li (transpose-words)))
  (keymap-set my-quick-edit-map "T" (li (transpose-words -1)))
  (with-eval-after-load 'consult
    (keymap-set my-quick-edit-map "y" #'consult-yank-replace))
  (with-eval-after-load 'yasnippet
    (keymap-set my-quick-edit-map "C-s" #'yas-insert-snippet))
  ;; Comment line insertion
  (with-eval-after-load 'custom-editing
    (keymap-set my-quick-edit-map "c"   #'my/edit-insert-comment-default)
    (keymap-set my-quick-edit-map "C-t" #'my/edit-insert-comment-default--top)
    (keymap-set my-quick-edit-map "C-b" #'my/edit-insert-comment-default--bottom)
    (keymap-set my-quick-edit-map "C"   #'my/edit-insert-comment)
    (keymap-set my-quick-edit-map "M-c" #'my/edit-insert-centered-comment-default)
    (keymap-set my-quick-edit-map "M-t" #'my/edit-insert-centered-comment-default--top)
    (keymap-set my-quick-edit-map "M-b" #'my/edit-insert-centered-comment-default--bottom)
    (keymap-set my-quick-edit-map "M-C" #'my/edit-insert-centered-comment)))

(my-quick-edit-map-setup)
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-c C-s" my-quick-edit-map))

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
  (evil-define-key 'normal magit-status-mode-map
    (kbd "M-s M-s") #'magit-stage
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-log-mode-map
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-stash-mode-map
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-revision-mode-map
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-diff-mode-map
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-stash-mode-map
    (kbd "e") #'vdiff-magit-dwim
    (kbd "E") #'vdiff-magit)
  (evil-define-key 'normal magit-revision-mode-map
    (kbd "e") #'vdiff-magit-dwim
    (kbd "E") #'vdiff-magit)
  (evil-define-key 'normal magit-commit-section-map
    (kbd "s") #'vdiff-magit-dwim))

(with-eval-after-load 'embark
  (evil-define-key 'normal embark-collect-mode-map
    (kbd "K") #'outline-previous-heading
    (kbd "J") #'outline-next-heading
    (kbd "H") #'outline-hide-body
    (kbd "L") #'outline-show-all))

(with-eval-after-load 'flycheck
  (evil-define-key 'normal flycheck-mode-map
    (kbd "C-f C-d") #'consult-flycheck
    (kbd "C-j") #'flycheck-next-error
    (kbd "C-k") #'flycheck-previous-error
    (kbd "M-j") #'compilation-next-error
    (kbd "M-k") #'compilation-previous-error
    ))

(with-eval-after-load 'flyspell
  (evil-define-key 'normal flyspell-mode-map
    (kbd "C-f C-d") #'consult-flyspell
    (kbd "M-j") #'flyspell-goto-next-error
    (kbd "M-k") #'flycheck-previous-error
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



(defun my-keymaps-set-agent-shell-global-keymaps ()
  (keymap-set my-utility-normal-map "L"   #'agent-shell-anthropic-start-claude-code)
  (keymap-set my-utility-normal-map "M-l" #'agent-shell-toggle)

  (keymap-set my-utility-normal-map "r" #'agent-shell-send-region)
  (keymap-set my-utility-normal-map "f" #'agent-shell-send-file)
  (keymap-set my-utility-normal-map "S" #'agent-shell-send-screenshot))

(my-keymaps-set-agent-shell-global-keymaps)

(provide 'mode-keymaps)
;;; mode-keymaps.el ends here
