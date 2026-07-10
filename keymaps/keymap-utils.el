;;; keymap-utils.el --- Keymap utility macros. -*- lexical-binding: t; -*-

;;; Commentary:
;; Declarative helpers for defining, binding, and populating keymaps.
;;
;; Has integration with evil-mode, can include `after' blocks to defer keymaps.
;;
;; Directives understood inside a body:
;;   :populate [STATE]              immediate section
;;   :after / :populate-after FEAT [STATE]
;;                                 section deferred via `with-eval-after-load'
;;   :evil-define STATE            (define-submaps only) sticky state for the
;;                                 submaps that follow

;;; Code:

;; -- Shared helpers (macro-expansion time) -------------------------------------

(defun my-keymaps--unquote (x)
  "Strip a leading `quote' from X."
  (if (and (consp x) (eq (car x) 'quote)) (cadr x) x))

;; Populate entries are always lists starting with a string key, so they
;; never collide with this test."
(defun my-keymaps--state-token-p (x)
  "Non-nil if X denotes an evil-state spec.
That is a quoted form (symbol or list) or a bare non-keyword symbol."
  (or (and (consp x) (eq (car x) 'quote))
      (and x (symbolp x) (not (keywordp x)))))

(defun my-keymaps--parse-plist (rest keys)
  "Consume leading `KEY VALUE' pairs from REST whose keys are in KEYS.
Return a cons (PLIST . REMAINING)."
  (let (plist)
    (while (and rest (memq (car rest) keys))
      (let ((k (pop rest)))
        (setq plist (plist-put plist k (pop rest)))))
    (cons plist rest)))

(defun my-keymaps--bind-form (map key def states)
  "Emit a binding form for KEY -> DEF in MAP honoring evil STATES.
With STATES (a state symbol or list) use `evil-define-key', else `keymap-set'."
  (if states
      `(evil-define-key ',states ,map (kbd ,key) ,def)
    `(keymap-set ,map ,key ,def)))

(defun my-keymaps--wk-form (map wk states)
  "Emit a which-key replacement form for MAP from flat WK (key/label list).
With evil STATES the labels are registered on the evil auxiliary keymap(s) so
they resolve for prefixes bound via `evil-define-key'.  Returns nil when WK is
empty.  Callers must ensure WK holds complete key/label pairs."
  (when wk
    (if states
        `(dolist (st ',(if (listp states) states (list states)))
           (which-key-add-keymap-based-replacements
             (evil-get-auxiliary-keymap ,map st t) ,@wk))
      `(which-key-add-keymap-based-replacements ,map ,@wk))))

(defun my-keymaps--parse-populate (body)
  "Parse BODY into a list of (FEATURE STATES ENTRIES) sections.
Recognizes :populate (immediate), :after/:populate-after FEAT (deferred).
Each directive may be followed by an evil-state spec.  Entries before any
directive form an immediate, state-less section."
  (let (sections feat states entries (rest body))
    (while rest
      (let ((item (pop rest)))
        (cond
         ((eq item :populate)
          (when entries (push (list feat states (nreverse entries)) sections))
          (setq feat nil states nil entries nil)
          (when (my-keymaps--state-token-p (car rest))
            (setq states (my-keymaps--unquote (pop rest)))))
         ((memq item '(:after :populate-after))
          (when entries (push (list feat states (nreverse entries)) sections))
          (setq feat (my-keymaps--unquote (pop rest)) states nil entries nil)
          (when (my-keymaps--state-token-p (car rest))
            (setq states (my-keymaps--unquote (pop rest)))))
         (t (push item entries)))))
    (when entries (push (list feat states (nreverse entries)) sections))
    (nreverse sections)))

(defun my-keymaps--emit-populate (map sections)
  "Generate binding forms for MAP from parsed SECTIONS."
  (let (forms)
    (dolist (section sections)
      (pcase-let ((`(,feat ,states ,entries) section))
        (let (binds wk)
          (dolist (entry entries)
            (pcase entry
              (`(,key ,(and (pred stringp) label))
               (push key wk) (push label wk))
              (`(,key ,cmd ,label)
               (push (my-keymaps--bind-form map key cmd states) binds)
               (push key wk) (push label wk))
              (`(,key ,cmd)
               (push (my-keymaps--bind-form map key cmd states) binds))))
          (let* ((wk-form (my-keymaps--wk-form map (nreverse wk) states))
                 (body (append (nreverse binds) (when wk-form (list wk-form)))))
            (when body
              (push (if feat
                        `(with-eval-after-load ',feat ,@body)
                      `(progn ,@body))
                    forms))))))
    (nreverse forms)))

(defun my-keymaps--emit-submap (parent-map spec states)
  "Emit forms defining the submap SPEC and binding it into PARENT-MAP.
STATES, when non-nil, binds the submap via `evil-define-key'.
SPEC is (SYM :prefix KEY [:doc STR] [:wk LABEL] ENTRIES...)."
  (let* ((sym    (car spec))
         (parsed (my-keymaps--parse-plist (cdr spec) '(:prefix :doc :wk)))
         (plist  (car parsed))
         (rest   (cdr parsed))
         (prefix (plist-get plist :prefix))
         (doc    (plist-get plist :doc))
         (wk     (plist-get plist :wk)))
    `(progn
       (defvar-keymap ,sym :doc ,(or doc ""))
       ,(my-keymaps--bind-form parent-map prefix sym states)
       ,@(when wk
           (let ((form (my-keymaps--wk-form parent-map (list prefix wk) states)))
             (when form (list form))))
       ,@(when rest
           (my-keymaps--emit-populate
            sym (my-keymaps--parse-populate rest))))))

;; ── Public macros ──────────────────────────────────────────────────────

(defun my-keymaps/exec-with-prefix (prefix)
  "Execute `execute-extended-command' with PREFIX pre-inserted in minibuffer."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert prefix))
    (command-execute #'execute-extended-command)))

(defmacro my-keymaps/populate (map &rest body)
  "Populate MAP with bindings.
BODY is entries optionally separated by directives.

Each entry is one of:
  (KEY CMD)       — bind only
  (KEY CMD LABEL) — bind + which-key label
  (KEY LABEL)     — which-key prefix label only (LABEL is a string)

Directives:
  :populate [STATE]            start a new immediate section
  :after / :populate-after FEAT [STATE]
                              defer following entries via `with-eval-after-load'

When a STATE (a state symbol or list of them) follows the directive, the
section's bindings use `evil-define-key'; otherwise `keymap-set'."
  (declare (indent 1))
  `(progn ,@(my-keymaps--emit-populate map (my-keymaps--parse-populate body))))

(defmacro my-keymaps/add-submaps (parent-map &rest body)
  "Register which-key labels for prefixes inside existing keymaps.
PARENT-MAP is informational only -- kept for visual symmetry with
`my-keymaps/define-submaps' but ignored at expansion time.

Each spec is:
  (EXISTING-MAP :after FEAT :prefix KEY [:doc STR] [:wk LABEL] [:evil STATE])

When :prefix and :wk are given, emits a deferred which-key replacement on
EXISTING-MAP.  With :evil STATE the label is registered on the evil auxiliary
keymap(s) for STATE, matching prefixes bound via `evil-define-key'."
  (declare (indent 1))
  (ignore parent-map)
  `(progn
     ,@(delq
        nil
        (mapcar
         (lambda (spec)
           (let* ((sym    (car spec))
                  (parsed (my-keymaps--parse-plist
                           (cdr spec) '(:after :prefix :doc :wk :evil)))
                  (plist  (car parsed))
                  (after  (my-keymaps--unquote (plist-get plist :after)))
                  (prefix (plist-get plist :prefix))
                  (wk     (plist-get plist :wk))
                  (states (my-keymaps--unquote (plist-get plist :evil)))
                  (form   (when (and prefix wk)
                            (my-keymaps--wk-form sym (list prefix wk) states))))
             (when form
               (if after `(with-eval-after-load ',after ,form) form))))
         body))))

(defmacro my-keymaps/define-submaps (parent-map &rest body)
  "Define submaps and bind them into PARENT-MAP.
BODY contains submap specs and directives, optionally followed by
:populate/:populate-after sections for PARENT-MAP itself.

Submap spec:
  (SYM :prefix KEY :doc STR [:wk LABEL]
    [:populate ENTRIES...]
    [:populate-after FEAT ENTRIES...])

Directive:
  :evil-define STATE   sticky -- bind the submaps that follow into PARENT-MAP
                       via `evil-define-key' in STATE (until changed/reset).

Top-level :populate/:populate-after populate PARENT-MAP and accept their own
trailing STATE just like `my-keymaps/populate'."
  (declare (indent 1))
  (let (forms parent-body (rest body) (states nil) in-parent)
    (while rest
      (let ((item (car rest)))
        (cond
         (in-parent (push (pop rest) parent-body))
         ((memq item '(:populate :populate-after))
          (setq in-parent t))
         ((eq item :evil-define)
          (pop rest)
          (setq states (my-keymaps--unquote (pop rest))))
         (t (push (my-keymaps--emit-submap parent-map (pop rest) states)
                  forms)))))
    (setq parent-body (nreverse parent-body))
    `(progn
       ,@(nreverse forms)
       ,@(when parent-body
           (my-keymaps--emit-populate
            parent-map (my-keymaps--parse-populate parent-body))))))

(provide 'keymap-utils)
;;; keymap-utils.el ends here
