;;; slot-layout.el --- Slot-based tiling window manager -*- lexical-binding: t; -*-

;; Author: natsukashii
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, frames, windows

;;; Commentary:
;; SLL (SLot-Layout) provides a declarative tiling window manager for Emacs.
;; It organizes windows into named "slots" with automatic buffer routing
;; based on major modes, buffer names, and regexp patterns.
;;
;; Key features:
;; - Declarative layout definition with tree-based splits.
;; - Automatic buffer routing.
;; - Sublayouts for quick slot combinations.
;; - Hide/show slots with state preservation.
;; - Auto-show hidden slots when routing buffers.
;;
;; Example:
;;   (sll-define-layout dev-3pane
;;     (sll-slots terminal sidebar)
;;     (sll-slot terminal
;;       :split (frame 'down 0.35)
;;       :modes (vterm-mode)
;;       :default-command vterm)
;;     (sll-slot sidebar
;;       :split (MAIN 'left 0.25)
;;       :modes (dired-mode))
;;     (sll-sublayouts term-only)
;;     (sll-sublayout term-only
;;       (terminal . vterm)))
;;
;;   (sll-load-layout 'dev-3pane)

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Custom Variables
;;; ============================================================================

(defgroup slot-layout nil
  "Slot-based window layout management."
  :group 'windows
  :prefix "sll-")

(defcustom sll-default-hide-strategy 'strict
  "Default strategy for showing slots with hidden parents.
- `strict': Parent must be shown first (default)
- `loose': Slot takes nearest visible ancestor's position"
  :type '(choice (const :tag "Strict (show parent first)" strict)
                 (const :tag "Loose (use nearest ancestor)" loose))
  :group 'slot-layout)

;;; ============================================================================
;;; State Variables (Public - inspectable via describe-variable)
;;; ============================================================================

(defvar sll-current-layout-name nil
  "Symbol name of the currently active layout, or nil.")

(defvar sll-current-sublayout-name nil
  "Symbol name of the current sublayout, or nil.")

(defvar sll-current-layout nil
  "The currently active layout plist, or nil.")

(defvar sll-current-tree nil
  "The tree structure of the current layout, or nil.")

(defvar sll-current-slots nil
  "Hash table of slot definitions in current layout, or nil.")

(defvar sll-current-hidden-slots nil
  "List of currently hidden slot names.")

(defvar sll-next-slot nil
  "When non-nil, next buffer routes to this slot. Auto-cleared after use.")

;;; ============================================================================
;;; Internal Variables
;;; ============================================================================

(defvar sll--layouts (make-hash-table :test 'eq)
  "Hash table: layout-name -> layout-plist.")

(defvar sll--slot-windows (make-hash-table :test 'eq)
  "Hash table: slot-name -> window object for current layout.")

(defvar sll--hidden-slot-state (make-hash-table :test 'eq)
  "Hash table: slot-name -> (:buffer :point :start) for hidden slots.")

(defvar sll--routing-installed nil
  "Non-nil when routing hooks are installed.")

;;; ============================================================================
;;; Tree Node Constructors
;;; ============================================================================

(defun sll--make-slot-node (name)
  "Create a slot node for NAME."
  (list :type 'slot :name name))

(defun sll--make-split-node (direction ratio first second)
  "Create a split node with DIRECTION, RATIO, FIRST and SECOND children."
  (list :type 'split
        :direction direction
        :ratio ratio
        :first first
        :second second))

(defun sll--slot-node-p (node)
  "Return t if NODE is a slot node."
  (and (listp node) (eq (plist-get node :type) 'slot)))

(defun sll--split-node-p (node)
  "Return t if NODE is a split node."
  (and (listp node) (eq (plist-get node :type) 'split)))

;;; ============================================================================
;;; Tree Building
;;; ============================================================================

(defun sll--find-and-replace-slot (tree target-name new-node)
  "In TREE, find slot TARGET-NAME and replace with NEW-NODE."
  (cond
   ((and (sll--slot-node-p tree)
         (eq (plist-get tree :name) target-name))
    new-node)
   ((sll--split-node-p tree)
    (sll--make-split-node
     (plist-get tree :direction)
     (plist-get tree :ratio)
     (sll--find-and-replace-slot (plist-get tree :first) target-name new-node)
     (sll--find-and-replace-slot (plist-get tree :second) target-name new-node)))
   (t tree)))

(defun sll--insert-split (tree target-slot new-slot-name direction ratio)
  "Insert NEW-SLOT-NAME into TREE by splitting TARGET-SLOT."
  (let* ((new-slot-node (sll--make-slot-node new-slot-name))
         (target-node (sll--make-slot-node target-slot))
         (new-is-first (memq direction '(left up)))
         (split-node (if new-is-first
                         (sll--make-split-node direction ratio new-slot-node target-node)
                       (sll--make-split-node direction ratio target-node new-slot-node))))
    (if (eq target-slot 'frame)
        (if new-is-first
            (sll--make-split-node direction ratio new-slot-node tree)
          (sll--make-split-node direction ratio tree new-slot-node))
      (sll--find-and-replace-slot tree target-slot split-node))))

(defun sll--build-tree (slot-plists)
  "Build layout tree from SLOT-PLISTS."
  (let ((tree (sll--make-slot-node 'MAIN)))
    (dolist (slot-plist slot-plists)
      (let* ((slot-name (plist-get slot-plist :name))
             (split (plist-get slot-plist :split))
             (target (or (plist-get split :target) 'MAIN))
             (direction (plist-get split :direction))
             (ratio (plist-get split :ratio)))
        (setq tree (sll--insert-split tree target slot-name direction ratio))))
    tree))

(defun sll--compute-slot-parents (slot-plists)
  "Compute parent for each slot from SLOT-PLISTS."
  (mapcar (lambda (slot-plist)
            (let* ((name (plist-get slot-plist :name))
                   (split (plist-get slot-plist :split))
                   (target (plist-get split :target)))
              (cons name (if (eq target 'frame) nil target))))
          slot-plists))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun sll-available-layouts ()
  "Return list of all defined layout names."
  (let (names)
    (maphash (lambda (k _v) (push k names)) sll--layouts)
    (nreverse names)))

(defun sll-available-slots ()
  "Return list of slot names in the current layout (including MAIN)."
  (if sll-current-layout-name
      (cons 'MAIN (copy-sequence (plist-get sll-current-layout :slots-order)))
    '(MAIN)))

(defun sll-visible-slots ()
  "Return list of currently visible slot names."
  (cl-set-difference (sll-available-slots) sll-current-hidden-slots))

(defun sll-available-sublayouts ()
  "Return list of sublayout names for current layout."
  (when sll-current-layout
    (let ((sublayouts (plist-get sll-current-layout :sublayouts))
          (names nil))
      (when sublayouts
        (maphash (lambda (k _v) (push k names)) sublayouts))
      (nreverse names))))

(defun sll--read-layout (prompt)
  "Read a layout name with completion using PROMPT."
  (intern (completing-read prompt
                           (mapcar #'symbol-name (sll-available-layouts))
                           nil t)))

(defun sll--read-slot (prompt)
  "Read a slot name with completion using PROMPT."
  (intern (completing-read prompt
                           (mapcar #'symbol-name (sll-available-slots))
                           nil t)))

(defun sll--read-visible-slot (prompt)
  "Read a visible slot name with completion using PROMPT."
  (let ((visible (sll-visible-slots)))
    (if visible
        (intern (completing-read prompt
                                 (mapcar #'symbol-name visible)
                                 nil t))
      (user-error "No visible slots"))))

(defun sll--read-hidden-slot (prompt)
  "Read a hidden slot name with completion using PROMPT."
  (if sll-current-hidden-slots
      (intern (completing-read prompt
                               (mapcar #'symbol-name sll-current-hidden-slots)
                               nil t))
    (user-error "No hidden slots")))

(defun sll--read-sublayout (prompt)
  "Read a sublayout name with completion using PROMPT."
  (let ((sublayouts (sll-available-sublayouts)))
    (if sublayouts
        (intern (completing-read prompt
                                 (mapcar #'symbol-name sublayouts)
                                 nil t))
      (user-error "No sublayouts defined"))))

(defun sll--empty-buffer-name (slot-name)
  "Return the empty buffer name for SLOT-NAME."
  (format "*%s-empty*" slot-name))

;;; ============================================================================
;;; Window Tracking
;;; ============================================================================

(defun sll--track-slot-window (slot window)
  "Track that SLOT is displayed in WINDOW."
  (puthash slot window sll--slot-windows)
  (set-window-parameter window 'sll-slot slot)
  (set-window-parameter window 'sll-layout sll-current-layout-name))

(defun sll--untrack-slot-window (slot)
  "Remove tracking for SLOT."
  (remhash slot sll--slot-windows))

(defun sll--get-slot-window (slot)
  "Get window for SLOT, validating it still exists."
  (let ((window (gethash slot sll--slot-windows)))
    (if (and window
             (window-live-p window)
             (eq (window-parameter window 'sll-slot) slot)
             (eq (window-parameter window 'sll-layout) sll-current-layout-name))
        window
      ;; Try to find by parameter search (fallback)
      (let ((found (cl-find-if
                    (lambda (w)
                      (and (eq (window-parameter w 'sll-slot) slot)
                           (eq (window-parameter w 'sll-layout) sll-current-layout-name)))
                    (window-list))))
        (when found
          (puthash slot found sll--slot-windows))
        found))))

(defun sll--split-window (window direction ratio)
  "Split WINDOW in DIRECTION. New window takes RATIO of space."
  (let* ((horizontal (memq direction '(left right)))
         (total (if horizontal
                    (window-total-width window)
                  (window-total-height window)))
         (original-size (max 1 (round (* total (- 1.0 ratio)))))
         (side (pcase direction
                 ('down 'below)
                 ('up 'above)
                 ('right 'right)
                 ('left 'left))))
    (split-window window original-size side)))

;;; ============================================================================
;;; Tree-Based Window Creation
;;; ============================================================================

(defun sll--create-windows-from-tree (tree window)
  "Create windows from TREE starting with WINDOW."
  (cond
   ((sll--slot-node-p tree)
    (let ((slot-name (plist-get tree :name)))
      (sll--track-slot-window slot-name window)
      (list (cons slot-name window))))

   ((sll--split-node-p tree)
    (let* ((direction (plist-get tree :direction))
           (ratio (plist-get tree :ratio))
           (first-child (plist-get tree :first))
           (second-child (plist-get tree :second))
           (new-is-first (memq direction '(left up)))
           (new-window (sll--split-window window direction ratio))
           (first-window (if new-is-first new-window window))
           (second-window (if new-is-first window new-window)))
      (append (sll--create-windows-from-tree first-child first-window)
              (sll--create-windows-from-tree second-child second-window))))))

;;; ============================================================================
;;; Buffer Routing Engine
;;; ============================================================================

(defun sll--match-buffer-to-slot (buffer)
  "Return slot name that should display BUFFER, or MAIN."
  (if (not sll-current-layout-name)
      'MAIN
    (let ((buf-name (buffer-name buffer))
          (buf-mode (buffer-local-value 'major-mode buffer))
          (matched-slot nil))
      (when sll-current-slots
        (maphash
         (lambda (slot-name slot-plist)
           (unless matched-slot
             (let ((modes (plist-get slot-plist :modes))
                   (names (plist-get slot-plist :names))
                   (regexps (plist-get slot-plist :regexps)))
               (when (or (memq buf-mode modes)
                         (member buf-name names)
                         (cl-some (lambda (re) (string-match-p re buf-name)) regexps))
                 (setq matched-slot slot-name)))))
         sll-current-slots))
      (or matched-slot 'MAIN))))

(defun sll--display-in-slot (buffer slot)
  "Display BUFFER in SLOT, auto-showing if hidden. Returns window."
  ;; Auto-show hidden slot
  (when (memq slot sll-current-hidden-slots)
    (sll-show-slot slot))
  ;; Get window and display
  (let ((window (sll--get-slot-window slot)))
    (when window
      (set-window-buffer window buffer))
    window))

(defun sll--display-buffer-function (buffer alist)
  "SLL display function for `display-buffer-alist'.
ALIST is ignored."
  (ignore alist)
  (when sll-current-layout-name
    (let* ((slot (if sll-next-slot
                     (prog1 sll-next-slot
                       (setq sll-next-slot nil))
                   (sll--match-buffer-to-slot buffer))))
      (sll--display-in-slot buffer slot))))

(defun sll--switch-to-buffer-advice (orig-fn buffer-or-name &rest args)
  "Advice to route `switch-to-buffer' through SLL routing."
  (if (and sll-current-layout-name
           (not (minibufferp))
           (not (string-prefix-p " " (if (bufferp buffer-or-name)
                                         (buffer-name buffer-or-name)
                                       (or buffer-or-name "")))))
      (let* ((buffer (if (bufferp buffer-or-name)
                         buffer-or-name
                       (get-buffer-create buffer-or-name)))
             (target-slot (if sll-next-slot
                              (prog1 sll-next-slot
                                (setq sll-next-slot nil))
                            (sll--match-buffer-to-slot buffer)))
             (current-slot (window-parameter (selected-window) 'sll-slot)))
        (if (eq target-slot current-slot)
            ;; Same slot - use original behavior
            (apply orig-fn buffer-or-name args)
          ;; Different slot - route through our system
          (let ((window (sll--display-in-slot buffer target-slot)))
            (when window
              (select-window window))
            buffer)))
    ;; No layout active or special buffer - use original
    (apply orig-fn buffer-or-name args)))

(defun sll--install-routing ()
  "Install SLL routing hooks."
  (unless sll--routing-installed
    ;; Add to display-buffer-alist
    (push '("." sll--display-buffer-function) display-buffer-alist)
    ;; Advise switch-to-buffer
    (advice-add 'switch-to-buffer :around #'sll--switch-to-buffer-advice)
    (setq sll--routing-installed t)))

(defun sll--uninstall-routing ()
  "Remove SLL routing hooks."
  (when sll--routing-installed
    (setq display-buffer-alist
          (cl-remove-if (lambda (entry)
                          (and (listp entry)
                               (eq (cadr entry) 'sll--display-buffer-function)))
                        display-buffer-alist))
    (advice-remove 'switch-to-buffer #'sll--switch-to-buffer-advice)
    (setq sll--routing-installed nil)))

;;; ============================================================================
;;; Slot Definition Parsing
;;; ============================================================================

(defun sll--parse-split (split-form)
  "Parse SPLIT-FORM into a structured plist."
  (unless (and (listp split-form) (= (length split-form) 3))
    (error "Invalid :split form: %S" split-form))
  (let ((target (nth 0 split-form))
        (direction (nth 1 split-form))
        (ratio (nth 2 split-form)))
    (when (and (listp direction) (eq (car direction) 'quote))
      (setq direction (cadr direction)))
    (unless (memq direction '(up down left right))
      (error "Invalid direction: %S" direction))
    (unless (and (numberp ratio) (> ratio 0) (< ratio 1))
      (error "Invalid ratio: %S" ratio))
    (list :target target :direction direction :ratio ratio)))

;;; ============================================================================
;;; Slot Definition Macros
;;; ============================================================================

(defmacro sll-slot (slot-name &rest args)
  "Define a slot named SLOT-NAME with properties from ARGS."
  (declare (indent 1))
  `(list ',slot-name
         (list :name ',slot-name
               :split (sll--parse-split ',(plist-get args :split))
               :modes ',(plist-get args :modes)
               :names ',(plist-get args :names)
               :regexps ',(plist-get args :regexps)
               :default-command ',(plist-get args :default-command)
               :hide-strategy ',(plist-get args :hide-strategy))))

(defmacro sll-slots (&rest slot-names)
  "Declare SLOT-NAMES as valid slots for the current layout."
  `(list 'sll--slots-declaration ',slot-names))

;;; ============================================================================
;;; Sublayout Macros
;;; ============================================================================

(defmacro sll-sublayouts (&rest names)
  "Declare sublayout NAMES for current layout."
  `(list 'sll--sublayouts-declaration ',names))

(defmacro sll-sublayout (name &rest slot-pairs)
  "Define sublayout NAME with SLOT-PAIRS.
Each pair is (slot-name . command-or-nil)."
  (declare (indent 1))
  `(list 'sll--sublayout-definition
         ',name
         ',slot-pairs))

;;; ============================================================================
;;; Layout Definition Macro
;;; ============================================================================

(defmacro sll-define-layout (layout-name &rest body)
  "Define a layout named LAYOUT-NAME with BODY."
  (declare (indent 1))
  (let ((docstring (when (stringp (car body)) (pop body))))
    `(progn
       (let* ((declarations (list ,@body))
              ;; Parse slots
              (slots-decl (cl-find-if
                           (lambda (d) (and (listp d) (eq (car d) 'sll--slots-declaration)))
                           declarations))
              (declared-slots (cadr slots-decl))
              (slot-defs (cl-remove-if
                          (lambda (d) (or (not (listp d))
                                          (memq (car d) '(sll--slots-declaration
                                                          sll--sublayouts-declaration
                                                          sll--sublayout-definition))))
                          declarations))
              ;; Parse sublayouts
              (sublayouts-decl (cl-find-if
                                (lambda (d) (and (listp d) (eq (car d) 'sll--sublayouts-declaration)))
                                declarations))
              (declared-sublayouts (cadr sublayouts-decl))
              (sublayout-defs (cl-remove-if-not
                               (lambda (d) (and (listp d) (eq (car d) 'sll--sublayout-definition)))
                               declarations))
              ;; Build data structures
              (slots-hash (make-hash-table :test 'eq))
              (slots-order nil)
              (slot-plists nil))
         
         ;; Validate and store slot definitions
         (dolist (def slot-defs)
           (let ((slot-name (car def))
                 (slot-plist (cadr def)))
             (unless (memq slot-name declared-slots)
               (error "Slot '%s' not declared in sll-slots" slot-name))
             (let* ((split (plist-get slot-plist :split))
                    (target (plist-get split :target)))
               (unless (or (eq target 'frame)
                           (eq target 'MAIN)
                           (memq target declared-slots))
                 (error "Invalid split target: %s" target)))
             (puthash slot-name slot-plist slots-hash)
             (push slot-name slots-order)
             (push slot-plist slot-plists)))
         
         ;; Build tree and compute parents
         (let* ((slots-order-final (nreverse slots-order))
                (slot-plists-final (nreverse slot-plists))
                (tree (sll--build-tree slot-plists-final))
                (parents (sll--compute-slot-parents slot-plists-final)))
           
           ;; Store parent info
           (dolist (pair parents)
             (let ((slot-plist (gethash (car pair) slots-hash)))
               (when slot-plist
                 (plist-put slot-plist :parent (cdr pair)))))
           
           ;; Build sublayouts hash
           (let ((sublayouts-hash (make-hash-table :test 'eq)))
             ;; Add 'full sublayout (all slots)
             (puthash 'full
                      (list :name 'full
                            :slots (mapcar (lambda (s) (cons s nil))
                                           (cons 'MAIN slots-order-final)))
                      sublayouts-hash)
             
             ;; Add user-defined sublayouts
             (dolist (def sublayout-defs)
               (let ((name (nth 1 def))
                     (pairs (nth 2 def)))
                 (when (and declared-sublayouts (not (memq name declared-sublayouts)))
                   (error "Sublayout '%s' not declared" name))
                 ;; Always include MAIN
                 (unless (assq 'MAIN pairs)
                   (push '(MAIN . nil) pairs))
                 (puthash name (list :name name :slots pairs) sublayouts-hash)))
             
             ;; Create and register layout
             (let ((layout (list :name ',layout-name
                                 :docstring ,docstring
                                 :slots-order slots-order-final
                                 :slots slots-hash
                                 :tree tree
                                 :sublayouts sublayouts-hash
                                 :default-sublayout 'full)))
               (puthash ',layout-name layout sll--layouts)
               (message "Defined layout: %s" ',layout-name)
               ',layout-name)))))))

;;; ============================================================================
;;; State Management
;;; ============================================================================

(defun sll--update-state-vars ()
  "Update public state variables."
  (if sll-current-layout-name
      (let ((layout (gethash sll-current-layout-name sll--layouts)))
        (setq sll-current-layout layout
              sll-current-tree (plist-get layout :tree)
              sll-current-slots (plist-get layout :slots)))
    (setq sll-current-layout nil
          sll-current-tree nil
          sll-current-slots nil
          sll-current-sublayout-name nil)))

(defun sll--clear-state ()
  "Clear all runtime state."
  (clrhash sll--slot-windows)
  (clrhash sll--hidden-slot-state)
  (setq sll-current-hidden-slots nil
        sll-next-slot nil))

;;; ============================================================================
;;; Execute in Slot
;;; ============================================================================

(defun sll-execute-in-slot (slot command)
  "Execute COMMAND with output displayed in SLOT.
SLOT is auto-shown if hidden."
  (interactive
   (list (sll--read-slot "Execute in slot: ")
         (read-command "Command: ")))
  (unless sll-current-layout-name
    (user-error "No layout active"))
  
  ;; Auto-show if hidden
  (when (memq slot sll-current-hidden-slots)
    (sll-show-slot slot))
  
  (let ((window (sll--get-slot-window slot)))
    (unless window
      (error "Cannot get window for slot '%s'" slot))
    (select-window window)
    (cond
     ((null command) nil)
     ((commandp command) (call-interactively command))
     ((functionp command) (funcall command))
     ((and (listp command) (not (eq (car command) 'lambda)))
      (eval command t))
     (t (funcall command)))))

;;; ============================================================================
;;; Layout Loading
;;; ============================================================================

;;;###autoload
(defun sll-load-layout (layout-name)
  "Load LAYOUT-NAME with all slots visible."
  (interactive (list (sll--read-layout "Load layout: ")))
  (let ((layout (gethash layout-name sll--layouts)))
    (unless layout
      (user-error "Layout '%s' not defined" layout-name))
    
    ;; Uninstall routing from previous layout
    (when sll-current-layout-name
      (sll--uninstall-routing))
    
    ;; Clear state
    (delete-other-windows)
    (sll--clear-state)
    
    ;; Set layout BEFORE creating windows
    (setq sll-current-layout-name layout-name)
    (sll--update-state-vars)
    
    ;; Install routing
    (sll--install-routing)
    
    ;; Create windows from tree
    (let ((tree (plist-get layout :tree)))
      (sll--create-windows-from-tree tree (selected-window)))
    
    ;; Run default commands for each slot
    (dolist (slot-name (cons 'MAIN (plist-get layout :slots-order)))
      (let* ((slot-plist (when (not (eq slot-name 'MAIN))
                           (gethash slot-name sll-current-slots)))
             (command (when slot-plist (plist-get slot-plist :default-command)))
             (window (sll--get-slot-window slot-name)))
        (when window
          (select-window window)
          (if command
              (condition-case err
                  (sll-execute-in-slot slot-name command)
                (error
                 (message "Error in default-command for '%s': %s"
                          slot-name (error-message-string err))
                 (switch-to-buffer (get-buffer-create
                                    (sll--empty-buffer-name slot-name)))))
            (switch-to-buffer (get-buffer-create
                               (sll--empty-buffer-name slot-name)))))))
    
    ;; Set sublayout to 'full
    (setq sll-current-sublayout-name 'full)
    
    ;; Select MAIN
    (let ((main-window (sll--get-slot-window 'MAIN)))
      (when main-window
        (select-window main-window)))
    
    (message "Loaded layout: %s" layout-name)))

;;;###autoload
(defun sll-switch-layout (layout-name)
  "Switch to LAYOUT-NAME, preserving matching buffers."
  (interactive (list (sll--read-layout "Switch to layout: ")))
  (let ((layout (gethash layout-name sll--layouts)))
    (unless layout
      (user-error "Layout '%s' not defined" layout-name))
    
    ;; Collect current buffers
    (let ((current-buffers (mapcar #'window-buffer (window-list)))
          (main-buffer (window-buffer (or (sll--get-slot-window 'MAIN)
                                          (selected-window)))))
      
      ;; Uninstall old routing
      (when sll-current-layout-name
        (sll--uninstall-routing))
      
      ;; Clear state
      (delete-other-windows)
      (sll--clear-state)
      
      ;; Set new layout
      (setq sll-current-layout-name layout-name)
      (sll--update-state-vars)
      
      ;; Install routing
      (sll--install-routing)
      
      ;; Create windows
      (let ((tree (plist-get layout :tree)))
        (sll--create-windows-from-tree tree (selected-window)))
      
      ;; Restore buffers to matching slots
      (dolist (slot-name (cons 'MAIN (plist-get layout :slots-order)))
        (let* ((slot-plist (when (not (eq slot-name 'MAIN))
                             (gethash slot-name sll-current-slots)))
               (window (sll--get-slot-window slot-name))
               (matching-buffer (when slot-plist
                                  (sll--find-matching-buffer current-buffers slot-plist))))
          (when window
            (select-window window)
            (cond
             ((eq slot-name 'MAIN)
              (when (buffer-live-p main-buffer)
                (switch-to-buffer main-buffer t t)))
             (matching-buffer
              (switch-to-buffer matching-buffer t t))
             (t
              (switch-to-buffer (get-buffer-create
                                 (sll--empty-buffer-name slot-name))))))))
      
      ;; Set sublayout
      (setq sll-current-sublayout-name 'full)
      
      ;; Select MAIN
      (let ((main-window (sll--get-slot-window 'MAIN)))
        (when main-window
          (select-window main-window))))
    
    (message "Switched to layout: %s" layout-name)))

(defun sll--find-matching-buffer (buffers slot-plist)
  "Find buffer from BUFFERS matching SLOT-PLIST."
  (let ((modes (plist-get slot-plist :modes))
        (names (plist-get slot-plist :names))
        (regexps (plist-get slot-plist :regexps)))
    (cl-find-if
     (lambda (buf)
       (when (buffer-live-p buf)
         (let ((buf-name (buffer-name buf))
               (buf-mode (buffer-local-value 'major-mode buf)))
           (or (memq buf-mode modes)
               (member buf-name names)
               (cl-some (lambda (re) (string-match-p re buf-name)) regexps)))))
     buffers)))

;;;###autoload
(defun sll-reset-layout ()
  "Reset current layout to initial state."
  (interactive)
  (if sll-current-layout-name
      (sll-load-layout sll-current-layout-name)
    (user-error "No layout active")))

;;;###autoload
(defun sll-unload-layout ()
  "Unload current layout and restore normal Emacs behavior."
  (interactive)
  (when sll-current-layout-name
    (sll--uninstall-routing)
    (sll--clear-state)
    (setq sll-current-layout-name nil)
    (sll--update-state-vars)
    (message "Layout unloaded")))

;;; ============================================================================
;;; Sublayout Switching
;;; ============================================================================

;;;###autoload
(defun sll-switch-sublayout (sublayout-name)
  "Switch to SUBLAYOUT-NAME.
Only hides/shows slots as needed - never kills buffers."
  (interactive (list (sll--read-sublayout "Switch to sublayout: ")))
  (unless sll-current-layout-name
    (user-error "No layout active"))
  
  (let* ((sublayouts (plist-get sll-current-layout :sublayouts))
         (sublayout (when sublayouts (gethash sublayout-name sublayouts))))
    (unless sublayout
      (user-error "Sublayout '%s' not defined" sublayout-name))
    
    (let* ((target-pairs (plist-get sublayout :slots))
           (target-slot-names (mapcar #'car target-pairs))
           (all-slots (cons 'MAIN (plist-get sll-current-layout :slots-order)))
           (slots-to-hide (cl-set-difference all-slots target-slot-names)))
      
      ;; Hide excess slots (except MAIN)
      (dolist (slot slots-to-hide)
        (unless (eq slot 'MAIN)
          (unless (memq slot sll-current-hidden-slots)
            (sll-hide-slot slot))))
      
      ;; Show needed slots and run commands
      (dolist (pair target-pairs)
        (let ((slot (car pair))
              (command (cdr pair)))
          ;; Show if hidden
          (when (memq slot sll-current-hidden-slots)
            (sll-show-slot slot))
          ;; Execute command if specified (nil = keep current buffer)
          (when command
            (sll-execute-in-slot slot command))))
      
      ;; Update sublayout name
      (setq sll-current-sublayout-name sublayout-name)
      
      ;; Focus MAIN
      (let ((main-window (sll--get-slot-window 'MAIN)))
        (when main-window
          (select-window main-window)))
      
      (message "Switched to sublayout: %s" sublayout-name))))

;;; ============================================================================
;;; Slot Navigation
;;; ============================================================================

;;;###autoload
(defun sll-switch-to-slot (slot)
  "Switch to SLOT window. Auto-shows if hidden."
  (interactive (list (sll--read-slot "Switch to slot: ")))
  (unless sll-current-layout-name
    (user-error "No layout active"))
  ;; Auto-show if hidden
  (when (memq slot sll-current-hidden-slots)
    (sll-show-slot slot))
  (let ((window (sll--get-slot-window slot)))
    (if window
        (select-window window)
      (user-error "Slot '%s' has no window" slot))))

;;;###autoload
(defun sll-send-buffer-to-slot (slot)
  "Move current buffer to SLOT."
  (interactive (list (sll--read-slot "Send buffer to slot: ")))
  (unless sll-current-layout-name
    (user-error "No layout active"))
  ;; Auto-show if hidden
  (when (memq slot sll-current-hidden-slots)
    (sll-show-slot slot))
  (let ((buffer (current-buffer))
        (window (sll--get-slot-window slot)))
    (unless window
      (user-error "Slot '%s' has no window" slot))
    (set-window-buffer window buffer)
    (select-window window)
    (message "Sent buffer '%s' to slot '%s'" (buffer-name buffer) slot)))

;;;###autoload
(defun sll-set-next-slot (slot)
  "Set SLOT as target for next buffer. Works for hidden slots."
  (interactive (list (sll--read-slot "Next buffer in slot: ")))
  (unless sll-current-layout-name
    (user-error "No layout active"))
  (setq sll-next-slot slot)
  (message "Next buffer will open in slot: %s" slot))

;;; ============================================================================
;;; Hide/Show Slots
;;; ============================================================================

;;;###autoload
(defun sll-hide-slot (slot)
  "Hide SLOT, preserving buffer state."
  (interactive (list (sll--read-visible-slot "Hide slot: ")))
  (unless sll-current-layout-name
    (user-error "No layout active"))
  (when (eq slot 'MAIN)
    (user-error "Cannot hide MAIN slot"))
  
  (let ((window (sll--get-slot-window slot)))
    (unless window
      (user-error "Slot '%s' has no window" slot))
    
    ;; Save state
    (puthash slot
             (list :buffer (window-buffer window)
                   :point (window-point window)
                   :start (window-start window))
             sll--hidden-slot-state)
    
    ;; Remove from tracking
    (sll--untrack-slot-window slot)
    
    ;; Delete window
    (delete-window window)
    
    ;; Update hidden list
    (cl-pushnew slot sll-current-hidden-slots)
    
    (message "Hidden slot: %s" slot)))

;;;###autoload
(defun sll-show-slot (slot)
  "Show hidden SLOT, restoring its state."
  (interactive (list (sll--read-hidden-slot "Show slot: ")))
  (unless sll-current-layout-name
    (user-error "No layout active"))
  (unless (memq slot sll-current-hidden-slots)
    (user-error "Slot '%s' is not hidden" slot))
  
  ;; Recreate window
  (let ((window (sll--recreate-slot-window slot)))
    (unless window
      (error "Failed to recreate window for slot '%s'" slot))
    
    ;; Track the new window
    (sll--track-slot-window slot window)
    
    ;; Restore buffer state
    (let ((saved (gethash slot sll--hidden-slot-state)))
      (select-window window)
      (if (and saved (buffer-live-p (plist-get saved :buffer)))
          (progn
            (switch-to-buffer (plist-get saved :buffer) t t)
            (goto-char (plist-get saved :point))
            (set-window-start window (plist-get saved :start)))
        (switch-to-buffer (get-buffer-create (sll--empty-buffer-name slot)))))
    
    ;; Update hidden list
    (setq sll-current-hidden-slots (delq slot sll-current-hidden-slots))
    (remhash slot sll--hidden-slot-state)
    
    (message "Restored slot: %s" slot)))

(defun sll--recreate-slot-window (slot)
  "Recreate window for SLOT based on split definition."
  (let* ((slot-plist (gethash slot sll-current-slots))
         (split (plist-get slot-plist :split))
         (target (plist-get split :target))
         (direction (plist-get split :direction))
         (ratio (plist-get split :ratio))
         (strategy (or (plist-get slot-plist :hide-strategy)
                       sll-default-hide-strategy))
         (target-window nil))
    
    (cond
     ;; Frame split
     ((eq target 'frame)
      (setq target-window (frame-root-window)))
     
     ;; MAIN - find it
     ((eq target 'MAIN)
      (setq target-window (sll--get-slot-window 'MAIN))
      (unless target-window
        ;; Fallback: find any visible slot window
        (maphash (lambda (_k v)
                   (when (and (not target-window) (window-live-p v))
                     (setq target-window v)))
                 sll--slot-windows)))
     
     ;; Another slot
     (t
      (setq target-window (sll--get-slot-window target))
      (unless target-window
        (pcase strategy
          ('strict
           (sll-show-slot target)
           (setq target-window (sll--get-slot-window target)))
          ('loose
           (let ((ancestor (plist-get slot-plist :parent)))
             (while (and ancestor (memq ancestor sll-current-hidden-slots))
               (let ((ancestor-plist (gethash ancestor sll-current-slots)))
                 (setq ancestor (when ancestor-plist
                                  (plist-get ancestor-plist :parent)))))
             (setq target-window
                   (if ancestor
                       (sll--get-slot-window ancestor)
                     (sll--get-slot-window 'MAIN)))))))))
    
    (unless target-window
      (error "Cannot find target window for slot '%s'" slot))
    
    (sll--split-window target-window direction ratio)))

;;;###autoload
(defun sll-toggle-slot (slot)
  "Toggle visibility of SLOT."
  (interactive (list (sll--read-slot "Toggle slot: ")))
  (if (memq slot sll-current-hidden-slots)
      (sll-show-slot slot)
    (sll-hide-slot slot)))

;;; ============================================================================
;;; Describe Functions
;;; ============================================================================

;;;###autoload
(defun sll-describe-slot (slot-or-window)
  "Describe SLOT-OR-WINDOW."
  (interactive
   (list (completing-read "Slot (or 'current-window'): "
                          (cons "current-window"
                                (mapcar #'symbol-name (sll-available-slots)))
                          nil t)))
  (let* ((slot (cond
                ((equal slot-or-window "current-window")
                 (window-parameter (selected-window) 'sll-slot))
                ((symbolp slot-or-window) slot-or-window)
                (t (intern slot-or-window))))
         (slot-plist (when (and slot sll-current-slots (not (eq slot 'MAIN)))
                       (gethash slot sll-current-slots))))
    
    (if (null slot)
        (message "Current window has no slot (layout: %s)"
                 (or sll-current-layout-name "none"))
      (with-help-window "*SLL Slot*"
        (princ (format "Slot: %s\n" slot))
        (princ (format "Layout: %s\n" (or sll-current-layout-name "none")))
        (princ (format "Visible: %s\n" (if (memq slot sll-current-hidden-slots) "no" "yes")))
        (when slot-plist
          (let ((split (plist-get slot-plist :split)))
            (princ (format "\nSplit: %s %s %.0f%%\n"
                           (plist-get split :target)
                           (plist-get split :direction)
                           (* 100 (plist-get split :ratio)))))
          (when (plist-get slot-plist :parent)
            (princ (format "Parent: %s\n" (plist-get slot-plist :parent))))
          (when (plist-get slot-plist :modes)
            (princ (format "Modes: %s\n" (plist-get slot-plist :modes))))
          (when (plist-get slot-plist :names)
            (princ (format "Names: %s\n" (plist-get slot-plist :names))))
          (when (plist-get slot-plist :regexps)
            (princ (format "Regexps: %s\n" (plist-get slot-plist :regexps))))
          (when (plist-get slot-plist :default-command)
            (princ (format "Default: %s\n" (plist-get slot-plist :default-command)))))
        (when (eq slot 'MAIN)
          (princ "\nMAIN is the primary editing slot (accepts unmatched buffers).\n"))))))

(defun sll--tree-to-string (tree &optional indent)
  "Convert TREE to string with INDENT level."
  (let* ((indent (or indent 0))
         (prefix (make-string (* indent 2) ?\s)))
    (cond
     ((sll--slot-node-p tree)
      (let* ((name (plist-get tree :name))
             (hidden (memq name sll-current-hidden-slots))
             (status (if hidden " [hidden]" "")))
        (format "%s[%s]%s\n" prefix name status)))
     
     ((sll--split-node-p tree)
      (let ((dir (plist-get tree :direction))
            (ratio (plist-get tree :ratio)))
        (concat
         (format "%s<split %s %.0f%%>\n" prefix dir (* 100 ratio))
         (sll--tree-to-string (plist-get tree :first) (1+ indent))
         (sll--tree-to-string (plist-get tree :second) (1+ indent)))))
     
     (t (format "%s[invalid node]\n" prefix)))))

;;;###autoload
(defun sll-describe-layout (&optional layout-name)
  "Describe LAYOUT-NAME or current layout."
  (interactive
   (list (when current-prefix-arg
           (sll--read-layout "Describe layout: "))))
  (let* ((name (or layout-name sll-current-layout-name))
         (layout (when name (gethash name sll--layouts))))
    (unless layout
      (user-error "No layout to describe"))
    (with-help-window "*SLL Layout*"
      (princ (format "Layout: %s\n" name))
      (when (plist-get layout :docstring)
        (princ (format "Description: %s\n" (plist-get layout :docstring))))
      (princ (format "Active: %s\n" (if (eq name sll-current-layout-name) "yes" "no")))
      (when (eq name sll-current-layout-name)
        (princ (format "Current sublayout: %s\n" sll-current-sublayout-name)))
      
      (princ "\nTree structure:\n")
      (princ (sll--tree-to-string (plist-get layout :tree)))
      
      (princ "\nSublayouts:\n")
      (let ((sublayouts (plist-get layout :sublayouts)))
        (when sublayouts
          (maphash (lambda (sub-name sub-def)
                     (princ (format "  %s: %s\n"
                                    sub-name
                                    (mapcar #'car (plist-get sub-def :slots)))))
                   sublayouts)))
      
      (princ "\nSlot definitions:\n")
      (dolist (slot-name (plist-get layout :slots-order))
        (let* ((slot (gethash slot-name (plist-get layout :slots)))
               (split (plist-get slot :split)))
          (princ (format "\n  %s:\n" slot-name))
          (princ (format "    Split: %s %s %.0f%%\n"
                         (plist-get split :target)
                         (plist-get split :direction)
                         (* 100 (plist-get split :ratio))))
          (when (plist-get slot :modes)
            (princ (format "    Modes: %s\n" (plist-get slot :modes))))
          (when (plist-get slot :default-command)
            (princ (format "    Default: %s\n" (plist-get slot :default-command)))))))))

;;; ============================================================================
;;; Convenience
;;; ============================================================================

(defun sll-current-layout ()
  "Return current layout name."
  sll-current-layout-name)

(defun sll-current-slot ()
  "Return slot of current window."
  (window-parameter (selected-window) 'sll-slot))

(provide 'slot-layout)
;;; slot-layout.el ends here
