;;; project-compile.el --- Declarative per-project compile commands. -*- lexical-binding: t; -*-

;;; Commentary:
;; A compact DSL for defining per-project compile commands from
;; `.project-locals.el' (see `project-setup.el').  Instead of writing a full
;; `(eval . (progn (defun ...) (local-set-key ...)))' block per command, set
;; the buffer-local variable `my-project/compile-commands' to a declarative
;; spec and this module generates the named interactive commands and binds
;; them to keys.
;;
;; Shape (the cdr of a `.project-locals.el' `nil' entry):
;;
;;   (my-project/compile-commands
;;    .
;;    ((:compile-command-overwrite ghostel-compile)  ; optional global-props list
;;
;;     (my-project-compile/build-and-run-target
;;      :buffer  "build-and-run"
;;      :command "cmake --build build --target filesystem"
;;               " && ./build/filesystem/filesystem"   ; strings are concatenated
;;      :keymap  "C-c C-b C-r")
;;
;;     (my-project-compile/build-target
;;      :buffer  "build"
;;      :command "cmake --build build --target filesystem"
;;      :keymap  "C-c C-b b")))
;;
;; Each command spec is `(FUNCTION-NAME PROP VALUE ...)':
;;   :buffer                 buffer-name suffix; the full name is built from the
;;                           backend's template in `my-project-compile-buffer-name-formats',
;;                           e.g. backend ghostel-compile + :buffer "build" ->
;;                           *ghostel-compile/build* (defaults to FUNCTION-NAME)
;;   :command                one or more strings, concatenated verbatim so a
;;                           long command line can be split for readability
;;   :keymap                 `kbd' string, bound locally to the command
;;   :run-from-project-root  t (default) runs from the project root; nil runs
;;                           from the visiting buffer's `default-directory'
;;   :compile-command        backend: `compile', `comint' or `ghostel-compile'
;;                           (default `ghostel-compile')
;;   :interactive            t makes a `ghostel-compile' buffer a writable
;;                           terminal you can type input into (`comint' already
;;                           accepts input; ignored by `compile')
;;
;; Global properties, given as a single plist list before the command specs:
;;   :compile-command-overwrite  force this backend for every command
;;
;; FUNCTION-NAME is defined as an interactive command, so it is also reachable
;; via `M-x'.  The spec is pure data (no `eval'), so it is accepted as a safe
;; file-local value without prompting.
;;
;; `my-project/recompile' re-runs the most recent command (any backend), into
;; the same buffer.

;;; Code:

(require 'cl-lib)
(require 'compile) ; compilation-{start,save-buffers-predicate,buffer-name-function}

;; Forward declaration so the `let' binding below is dynamic, not lexical.
;; The real definition (a defcustom) comes from the ghostel-compile package.
(defvar ghostel-compile-buffer-name)

;; =============================================================================
;; DECLARATIVE COMPILE-COMMAND DSL
;; =============================================================================

(defvar-local my-project/compile-commands nil
  "Declarative per-project compile-command spec.
Normally set through `.project-locals.el'.  See the file commentary
for the accepted shape.")

;; The value is inert data (processed by us, never eval'd by dir-locals), so
;; accept any list without prompting.
(put 'my-project/compile-commands 'safe-local-variable #'listp)

(defconst my-project-compile--backends
  '(compile comint ghostel-compile)
  "Recognised `:compile-command' backends.")

(defun my-project-compile--unquote (form)
  "Strip a leading `quote' from FORM, e.g. (quote x) -> x."
  (if (and (consp form) (eq (car form) 'quote))
      (cadr form)
    form))

(defun my-project-compile--root ()
  "Return the current project root, or `default-directory' as a fallback."
  (if-let ((proj (project-current)))
      (expand-file-name (project-root proj))
    default-directory))

(defvar my-project-compile-buffer-name-formats
  '((compile         . "*compile/%s*")
    (comint          . "*comint/%s*")
    (ghostel-compile . "*ghostel-compile/%s*"))
  "Buffer-name templates per backend.
Each entry is (BACKEND . FORMAT).  FORMAT is passed to `format' with the
command's :buffer suffix, so e.g. (compile . \"*compile/%s*\") names the
buffer of a :buffer \"build\" command *compile/build*.")

(defun my-project-compile--buffer-name (backend buffer)
  "Return the buffer name for the BUFFER suffix under BACKEND."
  (if-let ((fmt (alist-get backend my-project-compile-buffer-name-formats)))
      (format fmt buffer)
    (format "*%s/%s*" backend buffer)))

(defvar my-project-compile--last nil
  "Plist describing the most recent `my-project-compile--run' invocation.
Keys: :command :buffer :backend :dir :interactive.  Read by
`my-project/recompile'.")

(defun my-project-compile--split-spec (spec)
  "Split SPEC into (GLOBAL-PLIST . COMMAND-SPECS).
When the first element is a list whose car is a keyword, e.g.
`(:compile-command-overwrite ghostel-compile)', it is the GLOBAL-PLIST;
the remaining elements are the individual command specs.  Command specs
start with a (non-keyword) function-name symbol, so there is no ambiguity."
  (if (and (consp (car spec)) (keywordp (caar spec)))
      (cons (car spec) (cdr spec))
    (cons '() spec)))

(defun my-project-compile--group-plist (body)
  "Parse BODY (a plist whose values may repeat) into an alist.
Each keyword maps to the list of values that follow it up to the
next keyword, so e.g. `:command \"a\" \"b\"' yields (:command \"a\" \"b\")."
  (let ((alist '())
        key vals)
    (dolist (item body)
      (if (keywordp item)
          (progn
            (when key (push (cons key (nreverse vals)) alist))
            (setq key item vals '()))
        (push item vals)))
    (when key (push (cons key (nreverse vals)) alist))
    (nreverse alist)))

(defun my-project-compile--run (command buffer backend dir &optional interactive)
  "Run COMMAND (named BUFFER) via BACKEND from DIR.
BACKEND is one of `my-project-compile--backends'.  When INTERACTIVE is
non-nil and BACKEND is `ghostel-compile', the buffer is a writable
terminal you can type into (`comint' accepts input unconditionally).
Records the invocation in `my-project-compile--last' for
`my-project/recompile'."
  (let* ((root (my-project-compile--root))
         (default-directory (file-name-as-directory (expand-file-name dir)))
         (name (my-project-compile--buffer-name backend buffer))
         (compilation-buffer-name-function (lambda (_mode) name))
         ;; All three backends route saving through `save-some-buffers' with
         ;; this predicate, so scope prompts to this project's buffers only.
         (compilation-save-buffers-predicate
          (lambda ()
            (and buffer-file-name
                 (string-prefix-p root (expand-file-name buffer-file-name))))))
    (setq my-project-compile--last
          (list :command command :buffer buffer :backend backend
                :dir dir :interactive interactive))
    (pcase backend
      ('ghostel-compile
       (let ((ghostel-compile-buffer-name name)
             (compile-command command))
         (ghostel-compile command interactive)))
      ('comint
       (compilation-start command t))
      ('compile
       (let ((compile-command command))
         (compile command)))
      (_ (user-error "Unknown compile backend: %S" backend)))))

(defun my-project/recompile ()
  "Re-run the most recent project compile command.
Reuses the command, buffer, backend, directory and interactive flag
from the last `my-project-compile--run'."
  (interactive)
  (unless my-project-compile--last
    (user-error "No project compile command has been run yet"))
  (let ((p my-project-compile--last))
    (my-project-compile--run (plist-get p :command)
                             (plist-get p :buffer)
                             (plist-get p :backend)
                             (plist-get p :dir)
                             (plist-get p :interactive))))

(defun my-project-compile--ensure-prefix-keys (keymap key-str)
  "Clear non-prefix bindings on intermediate keys of KEY-STR in KEYMAP.
Mirrors the elastic-files behaviour so a leaf binding like \"C-c C-b b\"
can coexist with a self-insert or command bound at \"C-c C-b\"."
  (let* ((keys (kbd key-str))
         (len (length keys)))
    (when (> len 1)
      (dotimes (i (1- len))
        (let* ((prefix (substring keys 0 (1+ i)))
               (binding (lookup-key keymap prefix)))
          (when (and binding (not (keymapp binding)))
            (define-key keymap prefix nil)))))))

(defun my-project-compile--bind-key (key-str command)
  "Bind KEY-STR to COMMAND in the buffer's local map."
  (unless (current-local-map)
    (use-local-map (make-sparse-keymap)))
  (my-project-compile--ensure-prefix-keys (current-local-map) key-str)
  (local-set-key (kbd key-str) command)
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements key-str (symbol-name command))))

(defun my-project-compile--define (spec global)
  "Define the interactive command described by SPEC.
GLOBAL is the parsed global plist from `my-project-compile--split-spec'."
  (unless (and (consp spec) (symbolp (car spec)) (not (keywordp (car spec))))
    (user-error "Malformed compile-command spec: %S" spec))
  (let* ((name (car spec))
         (props (my-project-compile--group-plist (cdr spec)))
         (buffer (or (car (alist-get :buffer props)) (symbol-name name)))
         (command (mapconcat #'identity (alist-get :command props) ""))
         (keymap (car (alist-get :keymap props)))
         ;; Distinguish an explicit `:run-from-project-root nil' from absence.
         (from-root (if-let ((cell (assq :run-from-project-root props)))
                        (car (cdr cell))
                      t))
         ;; Non-nil makes a ghostel buffer a writable terminal (accepts input).
         (writable (car (alist-get :interactive props)))
         (backend (my-project-compile--unquote
                   (or (plist-get global :compile-command-overwrite)
                       (car (alist-get :compile-command props))
                       'ghostel-compile))))
    (when (string-empty-p command)
      (user-error "Compile command %s has no :command" name))
    (unless (memq backend my-project-compile--backends)
      (user-error "Compile command %s: unknown backend %S" name backend))
    (defalias name
      (lambda ()
        (interactive)
        (my-project-compile--run
         command buffer backend
         (if from-root (my-project-compile--root) default-directory)
         writable))
      (format "Project compile [%s]: %s" backend command))
    (when keymap
      (my-project-compile--bind-key keymap name))))

(defun my-project/compile-setup ()
  "Generate the commands and keybindings from `my-project/compile-commands'.
Runs after the `.project-locals.el' loader has set the variable."
  (when my-project/compile-commands
    (pcase-let ((`(,global . ,specs)
                 (my-project-compile--split-spec my-project/compile-commands)))
      (dolist (spec specs)
        (condition-case err
            (my-project-compile--define spec global)
          (error (message "project-compile: %s" (error-message-string err))))))))

;; Depth 90 keeps this after `my/load-project-locals' (depth 0), which sets
;; the buffer-local `my-project/compile-commands' from .project-locals.el.
(add-hook 'find-file-hook #'my-project/compile-setup 90)
(add-hook 'dired-mode-hook #'my-project/compile-setup 90)

;; =============================================================================
;; LEGACY HELPERS
;; =============================================================================
;; Kept for keymaps and .project-locals.el files that still call them directly.

(defun my/compile-with-name (name-suffix &optional command directory)
  "Run compilation with project-specific buffer named *NAME-SUFFIX:project*.
DIRECTORY, if non-nil, is the working directory for compilation (relative to
project root or absolute).  This ensures `default-directory' in the
compilation buffer matches where the build actually runs, so that
`consult-compile-error' and similar tools can find related buffers."
  (let* ((project (projectile-project-name))
         (project-root (expand-file-name (projectile-project-root)))
         (default-directory (if directory
                                (file-name-as-directory
                                 (expand-file-name directory project-root))
                              project-root))
         (compilation-buffer-name-function
          (lambda (_mode) (format "*%s:%s*" name-suffix project)))
         ;; Prevent compile from overwriting buffer-local compile-command
         (compile-command (or command compile-command)))
    ;; Ask to save only buffers belonging to this project, then suppress
    ;; compile's own save-some-buffers call (which would ask about everything).
    (save-some-buffers nil
                       (lambda ()
                         (and buffer-file-name
                              (string-prefix-p project-root
                                               (expand-file-name buffer-file-name)))))
    (cl-letf (((symbol-function 'save-some-buffers) #'ignore))
      (compile compile-command))))

(defun my/project-compile ()
  (interactive)
  (my/compile-with-name "compile" projectile-project-compilation-cmd))

(defun my/project-run ()
  (interactive)
  (my/compile-with-name "run" projectile-project-run-cmd))

(defun my/project-test ()
  (interactive)
  (my/compile-with-name "test" projectile-project-test-cmd))

(defun my/project-custom (name command &optional directory)
  "Run arbitrary named compilation.
DIRECTORY, if non-nil, sets the working directory (relative to project root)."
  (interactive "sBuffer name: \nsCommand: ")
  (my/compile-with-name name command directory))

(defun my/project-custom-comint (name command)
  "Run arbitrary named compilation with comint mode (interactive input)."
  (interactive "sBuffer name: \nsCommand: ")
  (let* ((project (projectile-project-name))
         (default-directory (projectile-project-root))
         (compilation-buffer-name-function
          (lambda (_mode) (format "*%s:%s*" name project))))
    ;; 't' as the mode argument to enable comint-mode
    (compilation-start command t)))

(dolist (var '(compile-command
               projectile-project-run-cmd
               my/project-custom-comint
               my/project-custom))
  (put var 'risky-local-variable nil)
  (put var 'safe-local-variable (lambda (_) t)))  ; Accept anything

(defun safe-concat (&rest strings)
  "Safely concatenate strings for use in dir-locals."
  (apply #'concat strings))

;; Mark it as safe
(put 'safe-concat 'safe-local-eval-function t)

(provide 'project-compile)
;;; project-compile.el ends here
