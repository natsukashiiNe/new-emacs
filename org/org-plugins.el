;;; org-plugins.el --- Plugins for org-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Plugins for org-mode and their configuration.

;;; Code:


;; dictionary
(use-package flyspell
  :ensure nil
  :hook (org-mode . flyspell-mode)  ;; Enable spell checking in Org mode
  :config
  (setq ispell-program-name "aspell") ;; Use Aspell
  (setq ispell-dictionary "en_US")
  (setq ispell-extra-args
        '("--sug-mode=ultra"
          "--lang=en_US"
          "--add-extra-dicts=/usr/lib/aspell/ru.multi"))
  (setq ispell-extra-args '("--sug-mode=ultra")) ;; Faster suggestions
  (setq flyspell-issue-message-flag nil))        ;; Prevent annoying messages
(setq ispell-program-name "aspell")              ;; Ensure Aspell is used

;; integration with vertigo
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))) ;; TODO

(use-package org-edna
  :ensure t
  :config
  (org-edna-mode 1))

;; TODO (require 'org-checklist)


;; Napking integration
(use-package ob-napkin
  :ensure t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(napkin . t))) ;; Adjust path if using pipx

(use-package org-drill
  :ensure t)

(use-package org-remark
  :ensure t
  :hook (org-mode . org-remark-mode)
  :bind (:map org-mode-map 
              ("C-c M d" . org-remark-mark-definition)
              ("C-c M f" . my/consult-definitions)
              ("C-f C-r" . my/consult-definitions)
              ("C-c M n" . org-remark-next)
              ("C-c M p" . org-remark-prev)
              ("C-c M r" . org-remark-remove))
  :config
  (org-remark-create "definition"
                     '(:inherit warning)
                     '(CATEGORY "definition")))

(defun my/consult-definitions ()
  "Find all org-remark DEFINITION marks with consult/vertico and live preview."
  (interactive)
  (require 'org-remark)
  (let* ((marks (seq-filter 
                 (lambda (ov) 
                   (and (overlay-get ov 'org-remark-label)
                        (string= (overlay-get ov 'org-remark-label) "definition")))
                 (overlays-in (point-min) (point-max))))
         (candidates
          (mapcar
           (lambda (mark)
             (let* ((beg (overlay-start mark))
                    (end (overlay-end mark))
                    (text (buffer-substring-no-properties beg end))
                    (line (line-number-at-pos beg))
                    (marker (set-marker (make-marker) beg)))
               (cons (format "%4d: %s" line text) marker)))
           marks)))
    (if candidates
        (let ((selected (consult--read 
                         candidates
                         :prompt "Jump to definition: "
                         :lookup #'consult--lookup-cdr
                         :state (lambda (action cand)
                                  (when (and cand (markerp cand))
                                    (if (eq action 'preview)
                                        (consult--jump cand)
                                      (when (eq action 'return)
                                        (consult--jump cand)))))
                         :sort nil)))
          (when (markerp selected)
            (goto-char selected)
            (recenter)
            (let* ((mark (seq-find 
                          (lambda (ov) 
                            (= (overlay-start ov) selected))
                          marks)))
              (when mark
                (pulse-momentary-highlight-region 
                 (overlay-start mark) 
                 (overlay-end mark) 
                 'highlight)))))
      (message "No definitions found in buffer"))))

;; LATEX

;; Auto-toggles LaTeX fragment previews as cursor moves in/out
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;;LaTeX editing in src blocks
(use-package auctex
  :ensure t
  :defer t)

;; Fast LaTeX insertion with tab-completion and templates

(use-package cdlatex
  :ensure t
  :hook (org-mode . org-cdlatex-mode)
  :config
  (setq cdlatex-math-symbol-prefix ?l)  
  (setq cdlatex-math-symbol-alist
        '((?a ("\\land"    "\\arccos"))
          (?o ("\\lor"               ))
          (?A ("\\forall"            ))
          (?w ("\\text"            ))
          (?E ("\\exists" "\\nexists"))))

  ;; Unbind TAB from cdlatex in org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (when (boundp 'org-cdlatex-mode-map)
                (define-key org-cdlatex-mode-map (kbd "TAB") nil)
                (define-key org-cdlatex-mode-map (kbd "<tab>") nil))))
  ;; Also unbind from regular cdlatex
  (with-eval-after-load 'cdlatex
    (define-key cdlatex-mode-map (kbd "TAB") nil)
    (define-key cdlatex-mode-map (kbd "<tab>") nil))

  :bind (:map org-mode-map
              ("C-l" . cdlatex-math-symbol)      ; backtick for symbols
              ("'" . cdlatex-math-modify)      ; quote for accents/fonts
              ("C-c {" . cdlatex-environment)) ; environment insertion
  )



(provide 'org-plugins)
;;; org-plugins.el ends here
