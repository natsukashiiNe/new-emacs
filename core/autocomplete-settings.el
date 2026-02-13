;;; autocomplete-settings.el --- Startup file -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings for auto-complete package (corfu).


;;; Code:

;; Enable native compilation if available

(use-package corfu
  :ensure t
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ("<tab>" . corfu-insert)
              ("C-SPC" . corfu-insert-separator))
  :custom
  ;; Auto completion
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)

  ;; Appearance
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-count 10)
  (corfu-scroll-margin 5)
  (corfu-max-width 80)
  (corfu-min-width 40)

  (corfu-popupinfo-delay '(0.2 . 0.2))  ; Show docs after 0.5s

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  
  :hook (lsp-bridge-mode . (lambda ()
                             (corfu-mode -1))))

(use-package corfu-terminal
  :ensure (:host codeberg :repo "akib/emacs-corfu-terminal")
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1))
  
  ;; Disable corfu-terminal in lsp-bridge modes
  ;; ACM handles auto-complete
  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (when (bound-and-true-p corfu-terminal-mode)
                (corfu-terminal-mode -1)))))

(use-package corfu-prescient
  :ensure t
  :after corfu
  :config
  (corfu-prescient-mode 1)
  (prescient-persist-mode 1))  ; Save frequency/recency data


(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  ;; order of the sources matters
  (add-to-list 'completion-at-point-functions #'cape-file)      ; Files
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)   ; Dynamic words
  (add-to-list 'completion-at-point-functions #'cape-keyword))  ; Programming keywords

(provide 'autocomplete-settings.el)
;;; autocomplete-settings ends here
