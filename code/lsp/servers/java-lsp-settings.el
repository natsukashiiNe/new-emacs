;;; java-lsp-settings.el --- Settings for java-ts-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;; Java language configuration: Eclipse JDT Language Server.

;;; Code:

;; --- Indent bars scope ---
(with-eval-after-load 'indent-bars
  (add-to-list 'indent-bars-treesit-scope
               '(java class_declaration method_declaration if_statement for_statement
                      while_statement switch_expression try_statement)))

;; --- Configuration Variables ---
(defcustom my/java-runtime-path "/usr/lib/jvm/java-23-openjdk"
  "Path to Java runtime for JDT language server."
  :type 'string
  :group 'my-lsp)

(defcustom my/java-format-settings-path "~/.java_formatting.xml"
  "Path to Java code formatting settings XML file."
  :type 'string
  :group 'my-lsp)

;; --- LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  (setq lsp-java-configuration-runtimes
        `[(:name "JavaSE-23"
		 :path ,my/java-runtime-path
		 :default t)])

  (setq lsp-java-format-settings-url
        (expand-file-name my/java-format-settings-path))

  (setq lsp-java-format-enabled t))

;; Hook setup
(add-hook 'java-ts-mode-hook #'lsp-deferred)
(add-hook 'java-mode-hook #'lsp-deferred)

(provide 'java-lsp-settings)
;;; java-lsp-settings.el ends here
