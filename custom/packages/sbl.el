;; (cl-defstruct sll--layout
;;   "Layout for SLL package."
;;   name window-tree elastic-frames
;;   )
;;

(cl-defstruct sll--slot
  "Named addressable slot to route buffers to.
Hashed by name."
  split-type    ; 'frame or '(slot <slot-name>)
  ;; TODO: do i make getter for this?
  split-dir     ; 'up 'down 'bottom 'left 'right, 'u 'd 'b 'l 'r
  size          ; absolute size (columns for v-split, rows for h-split)
  ;; TODO  it would used for a proper restoration
  ratio         ; 0.0-0.99, mutually exclusive with size
  buffer-modes
  buffer-regexps)

(defconst sll--horizontal-dirs '(left l right r))
(defconst sll--vertical-dirs '(up u top t down d bottom b))
(defconst sll--valid-split-dirs
  (append sll--horizontal-dirs sll--vertical-dirs))

;; TODO: is hashmap really the fastest on such small N?
(defvaralias 'sll--sr 'sll--slots-registry)
(defvar sll--slots-registry (make-hash-table :test #'equal))

(cl-defun sll--parent-dimension (&key split-type split-dir)
  "Return the parent dimension (width or height) for ration/size conversion.
SPLIT-TYPE is \='frame or a slot name string.
SPLIT-DIR is up/top or down/bottom."
  (let* ((is-hor-split (memq split-dir sll--horizontal-dirs)))
    (pcase split-type
      ('frame
       (if is-hor-split (frame-width) (frame-height)))
      (_
       ;; Parent is another slot - find its window and get its size
       (let* ((parent-win (sll--find-window-by-slot split-type)))
	 (if parent-win
	     (if is-hor-split (window-total-width parent-win)
	       (window-total-height parent-win))
	   (error "No window to split from is found!")))
       ))))

;; TODO: several hash-maps for different perpectives / sessions
;; TODO: when original window gets size and attrs (left and down|bottom)
;; splits - properly setup attributes.
;; TODO: exaand struct fields via macro
(cl-defun sll-make-slot (name &key split-type split-dir size ratio buffer-modes buffer-regexps)
  "Create a slot with the NAME.
SPLIT-TYPE is frame or name of the slot to create new window from.
SPLIT-DIR is one of the up/top, down/bottom (u t d b).
SIZE is size of the new window in pixels.
Can not be set with RATIO simultaneously.
RATIO is percentage of the new window relative to its parent (frame or slot).
Can not be set with SIZE simultaneously.
BUFFER-MODES is a list of modes to route buffers
with any of such modes to this slot.
BUFFER-REGEXPS is a list of regexps to route buffers
with any of such regexps names to this slot."
  (when (and size ratio)
    (error "Error while creating slot: sll-make-slot: cannot set both :size and :ratio"))
  (when split-dir
    (unless (memq split-dir sll--valid-split-dirs)
      (error
       (concat
	"Error while creating slot: sll-make-slot:"
	" invalid :split-dir `%S', expected one of %S")
       split-dir
       sll--valid-split-dirs)))
  ;; todo match to the win/slot split
  (let* ((is-hor-split (memq split-dir sll--horizontal-dirs))
	 ;; TODO make it split-type gnostic
	 (split-info (cons (split-type . split-dir )))
	 (dim (if is-hor-split (frame-width) (frame-height)))
	 (size (or size (when ratio (round (* dim ratio)))))
	 ;; (ratio (or ratio (when size (round (* ratio (unless (eq split-type 'frame) (if is-hor-split (frame-width) (frame-height) (if is-hor-split (window-width) (window-height)))))))))
	 ;; ;; FIX unless syntax
	 (let* ((slot (make-sll--slot
		       :split-type split-type
		       :split-dir split-dir
		       :size size
		       :ratio ratio
		       :buffer-modes buffer-modes
		       :buffer-regexps buffer-regexps)))
	   (puthash name slot sll--sr)
	   slot)))

  (defun sll--split-dir-to-size (dir size)
    "Convert DIR and SIZE to correct signed size for split function.
Negative means new window gets SIZE, positive means original gets SIZE."
    (pcase dir
      ((or 'bottom 'b 'down 'd) (- size))         ; new window below gets SIZE rows
      ((or 'up 'u) size)               ; original window gets SIZE rows
      ((or 'right 'r) (- size))        ; new window right gets SIZE cols
      ((or 'left 'l) size)))           ; original gets SIZE cols

  ;; TODO: this currently only works when splitting from ROOT.
  ;; probably would neeed to also accoutn for SPLIT-TYPE attr.
  (defun sll--split-fn-for-dir (dir)
    "Return the correct split function for DIR."
    (pcase dir
      ((memq dir sll--horizontal-dirs) #'split-root-window-below)
      ;; TODO at this point it all should be already validated
      ((memq dir sll--vertical-dirs) #'split-root-window-right)))

  (defun sll--find-window-by-slot (slot-name)
    "Find a live window with sll-slot-name equal to SLOT-NAME."
    (cl-find-if (lambda (win)
		  (equal (window-parameter win 'sll-slot-name) slot-name))
		(window-list)))

  (defun sll--resolve-size (slot)
    "Get absolute size from SLOT, deducting from ratio if needed."
    (or (sll--slot-size slot)
	(let* ((ratio (sll--slot-ratio slot))
	       (dir (sll--slot-split-dir slot))
	       (dim (if (memq dir '(left l right r))
			(frame-width)
		      (frame-height))))
	  (round (* dim ratio)))))

  (defun sll--create-or-find-slot (slot-name)
    "Find window with SLOT-NAME or create it.
Always moves focus to the slot window.  Returns the window."
    (let ((win (or (sll--find-window-by-slot slot-name)
		   (let ((new-win (split-root-window-below (round (* (frame-height) -0.35)))))
		     (set-window-parameter new-win 'sll-slot-name slot-name)
		     new-win))))
      (select-window win)
      win))

  (defun sll--create-window-for-slot (slot)
    "Create a new window for SLOT, set its parameter, return it."
    (let* ((dir (sll--slot-split-dir slot))
	   (size (sll--resolve-size slot))
	   ;; TODO this is resolved with sll-split-fn-for-dir
	   (split-fn (sll--split-fn-for-dir (dir)))
	   ;; TODO this is managed by sll-resolve-size
	   (signed-size (sll--split-dir-to-size))
	   (initial-win (selected-window))
	   (new-win (funcall split-fn signed-size))
	   (reverse (memq dir '(up u left l)))
	   (slot-win (if reverse initial-win new-win)))
      (set-window-parameter slot-win 'sll-slot-name (sll--slot-name slot))
      slot-win))


					; ============================== Slot utilization ==============================

  (cl-defun sll--exec-in-slot (&key name action focus)
    "Execute ACTION in window with NAME slot, creating it if needed.
If FOCUS is non-nil, keep focus on slot window.
If nil, return focus to original window."
    (let* ((cur-win (selected-window))
	   (act-win (sll--create-or-find-slot name)))
      (funcall action)
      (unless focus
	(run-at-time 0 nil `(lambda ()
			      (select-window ,cur-win))))))

  (sll--exec-in-slot
   :name "bottom-panel"
   :focus t
   :action (lambda ()
	     (if multi-vterm-buffer-list
		 (switch-to-buffer (car multi-vterm-buffer-list))
	       (multi-vterm))))

  ;; == OLD

  (defvar sll--slots
    (list
     (make-sll--slot :name "bottom-panel"
		     :split-type 'frame
		     :split-dir 'down
		     :ratio 0.35)
     (make-sll--slot :name "side-bar"
		     :split-type 'frame
		     :split-dir 'right
		     :size 30)))

  ;; == TEST

  (defun my-load-create-layout()

    )
