;;; exmodus-theme-gen.el --- Theme generator: one face list -> N themes -*- lexical-binding: t; -*-

;;; Commentary:
;; Engine that generates several themes from ONE list of faces.
;;
;; Three layers:
;;
;;   1. Palettes (`my-theme/palettes') -- sets of independent named
;;      colors, one for each named variant (i.e dark/light).
;;      Value of the color can be either a string or name of the other
;;      color of that set.
;;
;;   2. Faces (`my-theme/define-themes') -- spec of the format
;;
;;        (face-name
;;         :extend t                       ; common attributes
;;         :dark  (:foreground cyan-accent :background bg-cyan)
;;         :light (:foreground green-dimmer :background bg-green))
;;
;;      Symbols get resolved against the palette of the variant they are
;;      written under.
;;
;;      Attributes before :dark/:light are common to all variants.  The
;;      variant plist is merged on top of them, so a key repeated there
;;      overrides the common value -- nil included:
;;
;;        (face-name
;;         :extend t
;;         :dark  (:foreground cyan-accent :extend nil)  ; dark: extend nil
;;         :light (:foreground green-dimmer))            ; light: extend t
;;
;;      Merging works per top-level attribute: a nested value such as
;;      :underline (:color ...) or :box (...) is replaced as a whole, its
;;      keys are NOT merged with the common one.
;;
;;   3. Generator -- registers themes BASE-*.
;;
;; SCOPING / INHERITANCE.  Both themes write to the SAME face symbol,
;; and only one theme is always active.  Therefore, `:inherit my/own-face'
;; automatically points to the "correct" version:
;; Emacs resolves inheritance lazily, at the time of rendering.
;; The consequences:
;; - the order of declaration is not important, you can refer to
;;   a face defined further down the file;
;; - faces that do not exist yet (own ones, or from an unloaded package)
;;   are declared automatically (`my-theme/--ensure-faces').
;;
;; TODO markers: use `:light todo' (or `:light (todo "hint")').
;; Those variants are skipped and listed by `my-theme/report-todos'.

;;; Code:

(require 'cl-lib)
(require 'custom)

(defgroup my-theme nil
  "Theme generator from the common face pool."
  :group 'faces)

;; =============================================================================
;; Palletes
;; =============================================================================

(defvar my-theme/palettes nil
  "ALIST (VARIANT . ENTRIES), where ENTRIES -- alist (NAME VALUE).
VALUE -- color string or color name of the same palette.
Filled in the file of the theme.")

(defvar my-theme/base-themes nil
  "ALIST (VARIANT . BASE-THEME-SYMBOL) -- base theme, to extend upon.
Loads via `my-theme/load' before switching to the generated theme.")

(defun my-theme/variants ()
  "LIST of defined variants for palette."
  (mapcar #'car my-theme/palettes))

(defun my-theme/color (name variant)
  "Get color NAME of palette VARIANT resolving aliases."
  (let ((palette (cdr (assq variant my-theme/palettes)))
        (cur name)
        (seen nil))
    (unless palette
      (error "No such palette: `%s'" variant))
    (while (and cur (symbolp cur))
      (when (memq cur seen)
        (error "Alias cycle in palette %s: %s" variant cur))
      (push cur seen)
      (let ((row (assq cur palette)))
        (unless row
          (error "Palette `%s' does not have color `%s'" variant cur))
        (setq cur (cadr row))))
    (unless (stringp cur)
      (error "Invalid record `%s' in palette %s: %S" name variant cur))
    cur))

(defun my-theme/colors (variant)
  "Print palette VARIANT with resolved aliases."
  (interactive
   (list (intern (completing-read "Variant: " (my-theme/variants) nil t))))
  (let ((rows (mapcar (lambda (row)
                        (format "  %-18s %s" (car row)
                                (my-theme/color (car row) variant)))
                      (cdr (assq variant my-theme/palettes)))))
    (if (called-interactively-p 'any)
        (with-current-buffer (get-buffer-create "*my-theme palette*")
          (erase-buffer)
          (insert (format "Palette %s\n\n%s\n" variant (string-join rows "\n")))
          (display-buffer (current-buffer)))
      rows)))

;; =============================================================================
;; Attributes Scheme
;; =============================================================================

(defconst my-theme/--attr-kind
  '((:foreground         . color)
    (:background         . color)
    (:distant-foreground . color)
    (:overline           . color)    ; nil | t | COLOR
    (:strike-through     . color)    ; nil | t | COLOR
    (:underline          . nested)   ; nil | t | COLOR | plist
    (:box                . nested))  ; nil | t | COLOR | plist
  "How to resolve value the attributes.  Skip if not in this list.")

(defconst my-theme/--nested-schema
  '((:underline (:color . color) (:style . pass) (:position . pass))
    (:box       (:color . color) (:line-width . pass) (:style . pass)))
  "Full list of available keys inside composite attributes.")

(defconst my-theme/--known-attrs
  '(:family :foundry :width :height :weight :slant :inverse-video
	    :stipple :font :inherit :extend
	    :foreground :background :distant-foreground
	    :overline :strike-through :underline :box)
  "White list of available attributes of a face.")

(defconst my-theme/--magic-colors '(foreground-color background-color unspecified)
  "Symbols to not try to resolve in a palette.")

(defun my-theme/--color-slot (value variant ctx)
  "Resolve VALUE as color for VARIANT.  CTX - error message."
  (cond
   ((memq value '(nil t)) value)
   ((memq value my-theme/--magic-colors) value)
   ((stringp value) value)
   ((and (symbolp value) (not (keywordp value)))
    (condition-case err (my-theme/color value variant)
      (error (error "%s: %s" ctx (error-message-string err)))))
   (t (error "%s: is not a color: %S" ctx value))))

(defun my-theme/--nested-slot (attr value variant ctx)
  "Resolve compound ATTR (:underline / :box)."
  (cond
   ;; nil / t / COLOR
   ((or (memq value '(nil t))
        (stringp value)
        (and (symbolp value) (not (keywordp value))))
    (my-theme/--color-slot value variant ctx))
   ;; plist
   ((keywordp (car-safe value))
    (let ((schema (cdr (assq attr my-theme/--nested-schema))))
      (cl-loop for (k v) on value by #'cddr
               for kind = (or (cdr (assq k schema))
                              (error "%s: key %s is not available in %s (try: %S)"
                                     ctx k attr (mapcar #'car schema)))
               append (list k (if (eq kind 'color)
                                  (my-theme/--color-slot
                                   v variant (format "%s %s" ctx k))
                                v)))))
   (t (error "%s: unknown value %s: %S" ctx attr value))))

(defun my-theme/--resolve-plist (plist variant face)
  "Resolve colors of PLIST against palette VARIANT.  FACE is for messages only."
  (cl-loop for (attr value) on plist by #'cddr
           for ctx = (format "%s [%s] %s" face variant attr)
           do (unless (memq attr my-theme/--known-attrs)
                (error "%s: unknown face attribute `%s'" ctx attr))
           append (list attr
                        (pcase (cdr (assq attr my-theme/--attr-kind))
                          ('color  (my-theme/--color-slot value variant ctx))
                          ('nested (my-theme/--nested-slot attr value variant ctx))
                          (_       value)))))

;; =============================================================================
;; TODO-Markers
;; =============================================================================

(defvar my-theme/missing nil
  "List (VARIANT FACE . HINT) -- faces not defined for the variant.")

(defun my-theme/--todo-p (overlay)
  "Return (t . HINT) if OVERLAY is a todo marker, else nil."
  (cond
   ((eq overlay 'todo) (cons t nil))
   ((and (consp overlay) (eq (car overlay) 'todo))
    (cons t (cadr overlay)))
   (t nil)))

(defun my-theme/report-todos ()
  "Show all `todo' holes from the last generation."
  (interactive)
  (let ((by-variant (make-hash-table :test 'eq)))
    (pcase-dolist (`(,variant ,face . ,hint) (reverse my-theme/missing))
      (push (format "  %-38s %s" face (or hint "")) (gethash variant by-variant)))
    (with-current-buffer (get-buffer-create "*my-theme TODO*")
      (erase-buffer)
      (if (null my-theme/missing)
          (insert "No holes -- every face is defined in every variant.\n")
        (maphash (lambda (variant lines)
                   (insert (format "=== %s: %d ===\n%s\n\n"
                                   variant (length lines)
                                   (string-join (nreverse lines) "\n"))))
                 by-variant))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; =============================================================================
;; FACE SPEC ASSEMBLY
;; =============================================================================

(defun my-theme/--face-attrs (face props variant)
  "Common PROPS plus the :dark/:light overlay, all resolved for VARIANT.
Overlay keys override common ones.  Return nil if the variant is marked
as todo."
  (let* ((key (intern (format ":%s" variant)))
         (overlay (plist-get props key))
         (todo (my-theme/--todo-p overlay)))
    (if todo
        (progn (push (cons variant (cons face (cdr todo))) my-theme/missing)
               nil)
      (let ((base (cl-loop for (k v) on props by #'cddr
                           unless (memq k (mapcar (lambda (v)
                                                    (intern (format ":%s" v)))
                                                  (my-theme/variants)))
                           append (list k v))))
        (cl-loop for (k v) on overlay by #'cddr
                 do (setq base (plist-put base k v)))
        (when base
          (my-theme/--resolve-plist base variant face))))))

(defun my-theme/--ensure-faces (specs)
  "Declare faces from SPECS that do not exist yet.
Needed both for own faces (diagnostics/inline-*) and for faces of packages
not yet loaded when the theme loads.  Once the package runs its
`defface', the theme settings are applied on top."
  (pcase-dolist (`(,name . ,_) specs)
    (unless (facep name)
      (custom-declare-face
       name '((t nil))
       "Declared by `my-theme/define-themes' before its owner was loaded."))))

;; =============================================================================
;; GENERATOR
;; =============================================================================

(defun my-theme/--define-one (base variant specs)
  "Define and register theme BASE-VARIANT from SPECS."
  (let ((theme (intern (format "%s-%s" base variant)))
        (faces nil))
    (pcase-dolist (`(,name . ,props) specs)
      (when-let* ((attrs (my-theme/--face-attrs name props variant)))
        (push `(,name ((t ,@attrs))) faces)))
    (custom-declare-theme theme (custom-make-theme-feature theme)
                          (format "Generated: %s, variant %s." base variant))
    (apply #'custom-theme-set-faces theme (nreverse faces))
    (custom-theme-set-variables theme `(frame-background-mode ',variant))
    (provide-theme theme)
    theme))

(defun my-theme/define-themes-1 (base specs)
  "Functional form of `my-theme/define-themes'."
  (setq my-theme/missing nil)
  (my-theme/--ensure-faces specs)
  (prog1 (mapcar (lambda (variant) (my-theme/--define-one base variant specs))
                 (my-theme/variants))
    (when my-theme/missing
      (message "my-theme: %d unresolved TODOs -- M-x my-theme/report-todos"
               (length my-theme/missing)))))

(defmacro my-theme/define-themes (base &rest specs)
  "Build themes BASE-dark / BASE-light (one per variant of `my-theme/palettes').

Each spec:

  (FACE-NAME
   COMMON-ATTRIBUTES...
   :dark  (ATTRIBUTES...)    ; or  :dark todo
   :light (ATTRIBUTES...))   ; or  :light (todo \"hint\")

Keys in :dark/:light override common ones with the same key.
Symbols in color slots are looked up in the palette of the matching variant."
  (declare (indent 1))
  `(my-theme/define-themes-1 ,base ',specs))

;; =============================================================================
;; LOADING / SWITCHING
;; =============================================================================

(defvar my-theme/current nil
  "Last enabled variant.")

(defvar my-theme/after-load-hook nil
  "Hook run after a theme is enabled.  Set non-face variables here:
evil cursors, `highlight-parentheses-colors', flyover tints, etc.")

(defun my-theme/load (base variant)
  "Disable all themes, load the base theme for VARIANT and enable BASE-VARIANT."
  (interactive
   (list "exmodus"
         (intern (completing-read "Variant: " (my-theme/variants) nil t))))
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (when-let* ((base-theme (cdr (assq variant my-theme/base-themes))))
    (load-theme base-theme t))
  (enable-theme (intern (format "%s-%s" base variant)))
  (setq my-theme/current variant)
  (run-hooks 'my-theme/after-load-hook)
  variant)

(defun my-theme/toggle ()
  "Toggle dark <-> light."
  (interactive)
  (my-theme/load "exmodus" (if (eq my-theme/current 'dark) 'light 'dark)))

;; =============================================================================
;; CHECKING
;; =============================================================================

(defun my-theme/check (specs)
  "Run SPECS through every palette, return a list of errors (or \"OK\")."
  (let (bad)
    (dolist (variant (my-theme/variants))
      (pcase-dolist (`(,name . ,props) specs)
        (condition-case err (my-theme/--face-attrs name props variant)
          (error (push (format "%s / %s: %s" name variant
                               (error-message-string err))
                       bad)))))
    (if bad (string-join (nreverse bad) "\n") "OK")))

;; =============================================================================
;; BAKING TO A FILE  (optional)
;; =============================================================================
;; Runtime generation is handy while tuning, but drags the palette in at
;; startup.  Once the theme settles, bake it into plain *-theme.el files and
;; put them on `custom-theme-load-path'.

(defun my-theme/bake (base variant specs dir)
  "Write generated theme BASE-VARIANT into DIR as a plain *-theme.el."
  (let* ((theme (format "%s-%s" base variant))
         (file (expand-file-name (format "%s-theme.el" theme) dir))
         (faces nil))
    (pcase-dolist (`(,name . ,props) specs)
      (when-let* ((attrs (my-theme/--face-attrs name props variant)))
        (push `(,name ((t ,@attrs))) faces)))
    (with-temp-file file
      (insert (format ";;; %s-theme.el --- generated, do not edit -*- no-byte-compile: t -*-\n\n"
                      theme))
      (insert (format "(deftheme %s \"Generated from exmodus-faces.el.\")\n\n" theme))
      (insert (format "(custom-theme-set-faces\n '%s\n" theme))
      (dolist (spec (nreverse faces))
        (insert (format "  %S\n" spec)))
      (insert " )\n\n")
      (insert (format "(custom-theme-set-variables '%s '(frame-background-mode '%s))\n\n"
                      theme variant))
      (insert (format "(provide-theme '%s)\n" theme)))
    file))

(provide 'exmodus-theme-gen)
;;; exmodus-theme-gen.el ends here
