;;; mode-keymaps.el --- Mode-specific keymaps. -*- lexical-binding: t; -*-

;;; Commentary:
;; Contains keymaps that are active only in specific modes.

;;; Code:
;; == Emacs-native keymaps ============================================

(keymap-set global-map "C-x C-x" 'previous-buffer)
(keymap-set global-map "C-x X"   'next-buffer)
(keymap-set global-map "C-x l"   'eval-expression)

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


;; == COMPILE MAP =====================================================

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

(provide 'mode-keymaps)
;;; mode-keymaps.el ends here
