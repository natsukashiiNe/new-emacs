;;; org-settings.el --- Settings for org mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(setq package--init-file-ensured t)     ;; Workaround for package.el trying to autoload
(setq load-prefer-newer t)              ;; Always prefer newer built-in files
(setq org-modules nil)                  ;; Don't autoload extra Org modules
;; (add-hook 'org-mode-hook #'hide-mode-line-mode) ;; disable status line in org mode TODO make it just different
(setq org-use-property-inheritance t)
(setq org-startup-indented t)           ;; Pretty indentation

(setq org-ellipsis " 󰞖 ")               ;; Make collapsible sections look better
(setq org-hide-leading-stars t)         ;; Hide extra stars in headlines
(setq org-special-ctrl-a/e t)           ;; More predictable movement in lists
(setq org-use-speed-commands t)         ;; Speed commands (useful for large org files)
(setq org-export-preserve-breaks t)     ;; new line = always breaks line


(setq org-startup-with-inline-images t) ;; Show images when opening an Org file
(setq org-image-actual-width nil)       ;; Scale images to their actual width

;; Enable persistent todo states tracking
(setq org-log-done 'time)
(setq org-log-into-drawer t)  ;; Store logs in a drawer for a cleaner view
;; bullets instead of asteriks
;; (add-hook 'org-mode-hook 'org-indent-mode)


(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))  ;; Largest
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0)))))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '(" " "◉" "●" "󰧂" "󰘍")
        org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?•) (?- . ?➤))))

;; LATEX
;; LATEX CONFIGURATION
(setq org-pretty-entities t)         ;; Display symbols (like LaTeX-style)
(setq org-highlight-latex-and-related '(native script entities))
(setq org-pretty-entities-include-sub-superscripts nil)

;; For PDF export: use xelatex with Cyrillic support
(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[12pt]{article}
                  \\usepackage{fontspec}
                  \\setmainfont{GoMono Nerd Font}
                  \\newfontfamily\\cyrillicfont{GoMono Nerd Font}
                  \\setmonofont{GoMono Nerd Font}
                  \\usepackage[russian,english]{babel}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-latex-default-figure-position "H")

;; For inline previews: use dvipng with standard latex (faster, works reliably)
(setq org-preview-latex-default-process 'dvipng)
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

;; Auto-preview on file open
(setq org-startup-with-latex-preview t)

;; Plantuml
(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (setq plantuml-output-type "txt"))

;; Configure org-babel for PlantUML
(setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")

;; For PNG file output
(setq org-babel-default-header-args:plantuml
      '((:results . "file")
        (:exports . "results")))

;; BABEL CONFIGURATION
(add-to-list 'org-src-lang-modes '("cmake" . cmake-ts))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (lisp . t)
   (emacs-lisp . t)
   (latex . t)
   (plantuml . t)))

(setq org-babel-lisp-eval-fn 'sly-eval)  ; Use SLY to evaluate
(setq org-confirm-babel-evaluate nil)

;; (setq org-confirm-babel-evaluate nil)  ; Or use the selective function above
(setq org-babel-python-command "python3")
(setq org-babel-default-header-args:python
      '((:results . "output")))

(defun my-org-babel-ansi-colorize ()
  "Apply ANSI color codes to the results of the current babel block."
  (when (not org-export-current-backend)
    (let ((result-pos (org-babel-where-is-src-block-result)))
      (when result-pos
        (let ((beg (save-excursion
                     (goto-char result-pos)
                     (forward-line)
                     (point)))
              (end (save-excursion
                     (goto-char result-pos)
                     (org-babel-result-end))))
          (ansi-color-apply-on-region beg end))))))

(add-hook 'org-babel-after-execute-hook #'my-org-babel-ansi-colorize)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode) ;; Indent wrapped lines nicely
(add-hook 'org-mode-hook #'pixel-scroll-precision-mode)

(use-package good-scroll
  :ensure t
  :hook (org-mode . good-scroll-mode)
  :config
  (good-scroll-mode 1))


(provide 'org-settings)
;;; org-settings.el ends here
