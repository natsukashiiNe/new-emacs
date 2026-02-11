;;; test-lsp-bridge.el --- Test file for lsp-bridge mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;; reqs are:
;; pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging

(use-package markdown-mode
  :ensure t)

(use-package lsp-bridge
  :ensure (:host github :repo "manateelazycat/lsp-bridge"
		 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
		 :build (:not compile))
  :init
  ;; Manually add subdirectories to load-path before loading
  (let ((lsp-bridge-dir (expand-file-name "lsp-bridge" elpaca-repos-directory)))
    (add-to-list 'load-path lsp-bridge-dir)
    (add-to-list 'load-path (expand-file-name "acm" lsp-bridge-dir))
    (add-to-list 'load-path (expand-file-name "core" lsp-bridge-dir))
    (add-to-list 'load-path (expand-file-name "langserver" lsp-bridge-dir))
    (add-to-list 'load-path (expand-file-name "multiserver" lsp-bridge-dir)))
  
  (require 'lsp-bridge)
  (global-lsp-bridge-mode)
  
  :config
  (setq lsp-bridge-enable-hover-diagnostic t
        lsp-bridge-enable-signature-help t
        lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        acm-enable-doc t
        acm-enable-doc-markdown-render 'async
        acm-backend-lsp-candidate-min-length 0))

(provide 'test-lsp-bridge)
;;; test-lsp-bridge.el ends here
