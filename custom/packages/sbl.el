;; (cl-defstruct sll--layout
;;   "Layout for SLL package."
;;   name window-tree elastic-frames
;;   )
;; 

(cl-defstruct sll--slot
  "Named addressable slot to route buffers to.
Hashed by name."
  split-type    ; 'frame or '(slot <slot-name>)
  split-dir     ; 'up 'down 'bottom 'left 'right, 'u 'd 'b 'l 'r
  size          ; absolute size (columns for v-split, rows for h-split)
  ;; TODO  it would used for a proper restoration
  ratio         ; 0.0-0.99, mutually exclusive with size 
  buffer-modes
  buffer-regexps)

;; TODO: is hashmap really the fastest on such small N?
(defvar sll--slots-registry (make-hash-table :test #'equal))
(defvaralias 'sll--sr 'sll--slot-registry)

;; TODO: several hash-maps for different perpectives / sessions
;; TODO: when original window gets size and attrs (left and down|bottom)
;; splits - properly setup attributes.
(defun sll--make-slot (name &rest args)
  "Constructor wrapper that validates size/ratio exclusivity."
  (when (and (plist-get args :size) (plist-get args :ratio))
    (error "sll--slot: cannot set both :size and :ratio"))
  (let* ((slot (apply #'make-sll--slot args)))
    (puthash name slot sll--sr)
    slot))

(defun sll--split-dir-to-size (dir size)
  "Convert DIR and SIZE to correct signed size for split function.
Negative means new window gets SIZE, positive means original gets SIZE."
  (pcase dir
    ((or 'bottom 'b 'down 'd) (- size))         ; new window below gets SIZE rows
    ((or 'up 'u) size)               ; original window gets SIZE rows, new is above... 
    ((or 'right 'r) (- size))        ; new window right gets SIZE cols
    ((or 'left 'l) size)))           ; original gets SIZE cols

;; TODO: this currently only works when splitting from ROOT.
;; probably would neeed to also accoutn for SPLIT-TYPE attr.
(defun sll--split-fn-for-dir (dir)
  "Return the correct split function for DIR."
  (pcase dir
    ((or 'bottom 'b 'down 'd 'up 'u) #'split-root-window-below)
    ((or 'right 'r 'left 'l) #'split-root-window-right)))

(defun sll--find-window-by-slot (slot-name)
  "Find a live window with sll-slot-name equal to SLOT-NAME."
  (cl-find-if (lambda (win)
                (equal (window-parameter win 'sll-slot-name) slot-name))
              (window-list)))

(defun sll--resolve-size (slot)
  "Get absolute size from slot, deducting from ratio if needed."
  (or (sll--slot-size slot)
      (let* ((ratio (sll--slot-ratio slot))
             (dir (sll--slot-split-dir slot))
             (dim (if (memq dir '(left l right r))
                      (frame-width)
                    (frame-height))))
        (round (* dim ratio)))))


(defun sll--create-window-for-slot (slot)
  "Create a new window for SLOT, set its parameter, return it."
  (let* ((dir (sll--slot-split-dir slot))
         (size (sll--resolve-size slot))
         (split-fn (pcase dir
                     ((or 'down 'd 'up 'u) #'split-root-window-below)
                     ((or 'right 'r 'left 'l) #'split-root-window-right)))
         (signed-size (pcase dir
                        ((or 'down 'd) (- size))
                        ((or 'up 'u) (- (frame-height) size))
                        ((or 'right 'r) (- size))
                        ((or 'left 'l) (- (frame-width) size))))
         (initial-win (selected-window))
         (new-win (funcall split-fn signed-size))
         (reverse (memq dir '(up u left l)))
         (slot-win (if reverse initial-win new-win)))
    (set-window-parameter slot-win 'sll-slot-name (sll--slot-name slot))
    slot-win))

(defun sll--create-or-find-slot (slot-name)
  "Find window with SLOT-NAME or create it.
Always moves focus to the slot window. Returns the window."
  (let ((win (or (sll--find-window-by-slot slot-name)
                 (let ((new-win (split-root-window-below (round (* (frame-height) -0.35)))))
                   (set-window-parameter new-win 'sll-slot-name slot-name)
                   new-win))))
    (select-window win)
    win))

(cl-defun sll--exec-in-slot (&key name action focus)
  "Execute ACTION in window with NAME slot, creating it if needed.
If FOCUS is non-nil, keep focus on slot window. If nil, return focus to original window."
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
