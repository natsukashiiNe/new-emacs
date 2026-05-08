;;; keymap-utils.el --- Keymap utility macros. -*- lexical-binding: t; -*-

;;; Commentary:
;; Declarative helpers for defining, binding, and populating keymaps.

;;; Code:

;; ── Shared helpers (macro-expansion time) ──────────────────────────────

(defun my-keymaps--parse-populate (body)
  "Parse BODY into a list of (FEATURE . ENTRIES) sections.
Recognizes :populate (immediate), :after/:populate-after FEAT (deferred).
Entries before any directive are immediate."
  (let (sections current-after current-entries (rest body))
    (while rest
      (let ((item (pop rest)))
        (cond
         ((eq item :populate)
          (when current-entries
            (push (cons current-after (nreverse current-entries)) sections))
          (setq current-after nil current-entries nil))
         ((memq item '(:after :populate-after))
          (when current-entries
            (push (cons current-after (nreverse current-entries)) sections))
          (let ((raw (pop rest)))
            (setq current-after (if (and (listp raw) (eq (car raw) 'quote))
                                    (cadr raw)
                                  raw)
                  current-entries nil)))
         (t (push item current-entries)))))
    (when current-entries
      (push (cons current-after (nreverse current-entries)) sections))
    (nreverse sections)))

(defun my-keymaps--emit-populate (map sections)
  "Generate binding forms for MAP from parsed SECTIONS."
  (let (forms)
    (dolist (section sections)
      (let* ((feat (car section))
             (entries (cdr section))
             binds wk)
        (dolist (entry entries)
          (pcase entry
            (`(,key ,(and (pred stringp) label))
             (push key wk) (push label wk))
            (`(,key ,cmd ,label)
             (push `(keymap-set ,map ,key ,cmd) binds)
             (push key wk) (push label wk))
            (`(,key ,cmd)
             (push `(keymap-set ,map ,key ,cmd) binds))))
        (let ((body (append (nreverse binds)
                            (when wk
                              `((which-key-add-keymap-based-replacements
                                  ,map ,@(nreverse wk)))))))
          (when body
            (push (if feat
                      `(with-eval-after-load ',feat ,@body)
                    `(progn ,@body))
                  forms)))))
    (nreverse forms)))

;; ── Public macros ──────────────────────────────────────────────────────

(defun my-keymaps/exec-with-prefix (prefix)
  "Execute `execute-extended-command' with PREFIX pre-inserted in minibuffer."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert prefix))
    (command-execute #'execute-extended-command)))

(defmacro my-keymaps/populate (map &rest body)
  "Populate MAP with bindings.
BODY is entries optionally separated by :after FEATURE directives.

Each entry is one of:
  (KEY CMD)       — bind only
  (KEY CMD LABEL) — bind + which-key label
  (KEY LABEL)     — which-key prefix label only (LABEL is a string)

Use :after FEATURE to defer following entries via `with-eval-after-load'."
  (declare (indent 1))
  `(progn ,@(my-keymaps--emit-populate map (my-keymaps--parse-populate body))))

(defmacro my-keymaps/add-submaps (parent-map &rest body)
  "Register which-key labels for prefixes inside existing keymaps.
PARENT-MAP is informational only -- kept for visual symmetry with
`my-keymaps/define-submaps' but ignored at expansion time.

Each spec is:
  (EXISTING-MAP :after FEAT :prefix KEY [:doc STR] [:wk LABEL])

When :prefix and :wk are given, emits a deferred
`which-key-add-keymap-based-replacements' on EXISTING-MAP."
  (declare (indent 1))
  (ignore parent-map)
  `(progn
     ,@(delq
        nil
        (mapcar
         (lambda (spec)
           (let* ((sym (car spec)) (rest (cdr spec))
                  after prefix doc wk)
             (ignore doc)
             (while (and rest (memq (car rest) '(:after :prefix :doc :wk)))
               (pcase (pop rest)
                 (:after  (setq after  (pop rest)))
                 (:prefix (setq prefix (pop rest)))
                 (:doc    (setq doc    (pop rest)))
                 (:wk     (setq wk     (pop rest)))))
             (let* ((feat (if (and (listp after) (eq (car after) 'quote))
                              (cadr after)
                            after))
                    (form (when (and prefix wk)
                            `(which-key-add-keymap-based-replacements
                              ,sym ,prefix ,wk))))
               (when form
                 (if feat `(with-eval-after-load ',feat ,form) form)))))
         body))))

(defmacro my-keymaps/define-submaps (parent-map &rest body)
  "Define submaps and bind them into PARENT-MAP.
BODY contains submap specs, then optional :populate/:populate-after sections
for the PARENT-MAP itself.

Submap spec:
  (SYM :prefix KEY :doc STR [:wk LABEL]
    [:populate ENTRIES...]
    [:populate-after FEAT ENTRIES...])

Top-level :populate/:populate-after populates PARENT-MAP."
  (declare (indent 1))
  (let (submap-specs parent-body (rest body) in-parent)
    ;; Split submap specs from parent populate body
    (while rest
      (if (or in-parent (memq (car rest) '(:populate :populate-after)))
          (progn (setq in-parent t)
                 (push (pop rest) parent-body))
        (push (pop rest) submap-specs)))
    (setq submap-specs (nreverse submap-specs)
          parent-body  (nreverse parent-body))
    `(progn
       ,@(mapcar
          (lambda (spec)
            (let* ((sym  (car spec))
                   (rest (cdr spec))
                   prefix doc wk)
              ;; Consume plist keys
              (while (and rest (memq (car rest) '(:prefix :doc :wk)))
                (pcase (pop rest)
                  (:prefix (setq prefix (pop rest)))
                  (:doc    (setq doc    (pop rest)))
                  (:wk     (setq wk     (pop rest)))))
              ;; rest is now the per-submap populate body
              `(progn
                 (defvar-keymap ,sym :doc ,(or doc ""))
                 (keymap-set ,parent-map ,prefix ,sym)
                 ,@(when wk
                     `((which-key-add-keymap-based-replacements
                         ,parent-map ,prefix ,wk)))
                 ,@(when rest
                     (my-keymaps--emit-populate
                      sym (my-keymaps--parse-populate rest))))))
          submap-specs)
       ,@(when parent-body
           (my-keymaps--emit-populate
            parent-map (my-keymaps--parse-populate parent-body))))))

(provide 'keymap-utils)
;;; keymap-utils.el ends here
