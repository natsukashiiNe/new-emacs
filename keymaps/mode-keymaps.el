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

(defvar-keymap my-quick-insert-map
  :doc "Map with to quickly insert often used text.")
(keymap-set global-map "C-c C-s" my-quick-insert-map)


;; TODO: redo in evil-normal / general
(defvar-keymap my-goto-map
  :doc "Map to quickly navigate files (globally).")
(keymap-set global-map "C-c h" my-goto-map)

;; ==== Emacs-(almost)-build-in keymaps ============================================
(keymap-set global-map "C-x C-b" #'projectile-ibuffer)
(keymap-set global-map "C-x B"   #'ibuffer)
(keymap-set global-map "C-x C-x" #'previous-buffer)
(keymap-set global-map "C-x X"   #'next-buffer)
(keymap-set global-map "C-x l"   #'eval-expression)
(keymap-set global-map "C-c E"   #'eplaca-log)

(with-eval-after-load 'consult
  (keymap-set evil-normal-state-map "C-s" 'consult-line)
  (keymap-set evil-visual-state-map "C-s" 'consult-line))

(defvar-keymap my-interface-map
  :doc "My interface keymap"
  "l" #'display-line-numbers-mode)
(keymap-set global-map "C-x i" my-interface-map)

(keymap-set minibuffer-local-map "C-S-v" 'evil-paste-before)
(keymap-set minibuffer-local-map "<escape>" 'abort-recursive-edit)
(keymap-set minibuffer-local-map
            "C-c o"
            (lambda ()
              (interactive)
              (my/set-vertico-count
               (+ vertico-count 10))))

(keymap-set minibuffer-local-map
            "C-c w"
            (lambda ()
              (interactive)
              (my/set-vertico-count
               (max 1 (- vertico-count 10)))))


;; == PROJECTILE ======================================================

;; == GOTO ============================================================

(defun my-keymaps-set-activities-global-keymaps ()
  (keymap-set my-goto-map "c" #'activities-new)
  (keymap-set my-goto-map "C" #'activities-define)
  (keymap-set my-goto-map "o" #'activities-switch)
  (keymap-set my-goto-map "O" #'activities-resume)
  (keymap-set my-goto-map "s" #'activities-suspend)
  (keymap-set my-goto-map "k" #'activities-kill)
  (keymap-set my-goto-map "h" #'activities-switch-buffer)
  (keymap-set my-goto-map "u" #'activities-revert)
  (keymap-set my-goto-map "r" #'activities-rename)
  (keymap-set my-goto-map "l" #'activities-list))

;; == COMPILE MAP =====================================================

(with-eval-after-load 'evil
  (defvar-keymap my-compile-map
    :doc "My compile keymap"
    "g" #'first-error)
  (keymap-set global-map "C-c C-c"  my-compile-map))

(defun define-my-compile-map ()
  "Defines keymaps for the MY-COMPILE-MAP."
  (keymap-set evil-normal-state-map "C-c C" 'my/project-custom-comint))

(define-my-compile-map)


;; == QUICK INSERT MAP ================================================
(defun my-quick-insert-map-setup ()
  (with-eval-after-load 'consult
    (keymap-set my-quick-insert-map "y" #'consult-yank-replace))
  (with-eval-after-load 'yasnippet
    (keymap-set my-quick-insert-map "C-s" #'yas-insert-snippet)))

(my-quick-insert-map-setup)

;; == ORG MODE ========================================================
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "C-j") #'org-next-visible-heading
    (kbd "C-k") #'org-previous-visible-heading))

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
    (kbd "<up>") nil)
  )



(defun my-keymaps-set-agent-shell-global-keymaps ()
  (keymap-set my-utility-normal-map "L"   #'agent-shell-anthropic-start-claude-code)
  (keymap-set my-utility-normal-map "M-l" #'agent-shell-toggle)

  (keymap-set my-utility-normal-map "r" #'agent-shell-send-region)
  (keymap-set my-utility-normal-map "f" #'agent-shell-send-file)
  (keymap-set my-utility-normal-map "S" #'agent-shell-send-screenshot))

(my-keymaps-set-agent-shell-global-keymaps)

(provide 'mode-keymaps)
;;; mode-keymaps.el ends here
