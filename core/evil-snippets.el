;;; evil-snippets.el --- Basic settings for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; General GUI settings, common packages and dependencies, qol.

;;; Code:

(defvar evil-start-recording-macro-hook nil
  "Hook run when Evil starts recording a macro.")
(defvar evil-stop-recording-macro-hook nil
  "Hook run when Evil stops recording a macro.")

;; Add the advice with debugging
(advice-add 'evil-record-macro :around
            (lambda (orig-fun &rest args)
              (let ((was-recording evil-this-macro)
                    (register evil-this-register))
                (message "BEFORE: was-recording=%s, evil-this-macro=%s, register=%s" 
                         was-recording evil-this-macro register)
                (apply orig-fun args)
                (message "AFTER: evil-this-macro=%s" evil-this-macro)
                (cond
                 ;; Started recording (wasn't recording before, is now)
                 ((and (not was-recording) evil-this-macro)
                  (message "TRIGGERING START HOOK")
                  (run-hooks 'evil-start-recording-macro-hook))
                 ;; Stopped recording (was recording before, isn't now)
                 ((and was-recording (not evil-this-macro))
                  (message "TRIGGERING STOP HOOK")
                  (run-hooks 'evil-stop-recording-macro-hook))))))

;; Add test functions to the hooks
(add-hook 'evil-start-recording-macro-hook
          (lambda ()
            (message "Started recording Evil macro in register: %s"
                     (if evil-this-register 
                         (char-to-string evil-this-register)
                       "unknown"))))

(add-hook 'evil-stop-recording-macro-hook
          (lambda ()
            (message "Stopped recording Evil macro")))


(provide 'evil-snippets)
;;; evil-snippets.el ends here
