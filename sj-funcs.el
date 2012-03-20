;;; sj-funcs --- Various useful functions
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-11-06

(defun sj/rotate-list (l &optional n direction)
  "Rotate list `l' by `n' places in direction `direction'.
Rotates to the left unless `direction' is 'right.  `n' defaults to 1."
  (when l
    (case direction
      ('right (append (last l n) (butlast l n)))
      (t (let ((n (or n 1))
	       (tail nil))
	   (while (> n 0)
	     (setq tail (cons (car l) tail)
		   l (cdr l)
		   n (- n 1)))
	   (nconc l tail))))))

(defun sj/span-list (pred l)
  "Returns (head . tail) where head is the prefix of `l' where `pred' holds
and tail is the remainder of the list `l'."
  (let (head)
    (while (and l (funcall pred (car l)))
      (setq head (cons (car l) head)
	    l (cdr l)))
    (cons (nreverse head) l)))

(defun sj/take-while (pred l)
  "Returns prefix of list `l' where predicate `pred' holds."
  (car (sj/span-list pred l)))

(defun sj/drop-while (pred l)
  "Drops prefix of list `l' where predicate `pred' holds."
  (cdr (sj/span-list pred l)))

(defun sj/copy-login-env-vars (vars &optional precmd)
  "Copy the values of the environment variables named in `vars' to Emacs
environment.

`vars' can be a list of strings or a string. `precmd' if non-nil should be
a string specifying a shell command to execute before captuing the values."
  (when vars
    (cond ((stringp vars)
	   (setenv vars)		; null it out first
	   (setenv vars (sj/get-shell-env-var vars precmd)))
	  ((listp vars)
	   (sj/copy-login-env-vars (car vars) precmd)
	   (sj/copy-login-env-vars (cdr vars) precmd))
	  (t (error "Argument must be a string or list of strings")))))

;; This should really be in project-root itself
(defun sj/project-root-rgrep ()
  "Run the rgrep command from the current project root."
  (interactive)
  (with-project-root (call-interactively 'rgrep)))

;; Originally by Stephen Gildea, Nov. 88
;; From the LCD -- ~/as-is/swap-wins.el
(defun sj/swap-window-positions ()
  "Swap the positions of this window and the next one.
gildea Nov 88"
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
	  (other-window-hscroll (window-hscroll other-window))
	  (other-window-point (window-point other-window))
	  (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))

(defun sj/toggle-debug-on-error ()
  "Toggle the value of debug-on-error."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to %s" debug-on-error))

(defun sj/copy-keys-from-keymap (from-map keys &optional to-map)
  "Copy the definitions of key sequences in `keys' from `from-map' to `to-map'.
A new keymap is created if `to-map' is nil.  `keys' should be a
list of the keys whose bindings are to be copied.  Each entry may
also be of the form (from-key . to-key) if the keys differ in the
two keymaps.

Example:
  (\"a\" [backspace]
   (\"v\"  . \"k\")
   ([?v] . [?\C-o])
   (\"\C-y\" . \"x\"))

The keymap will have `from-map's bindings for \"v\" on \"k\" and \"\C-o\",
and the binding for \"\C-y\" on \"x\". The bindings for \"a\" and [backspace]
will be copied as well."
  (let ((new-map (or to-map (make-sparse-keymap))))
    (dolist (entry keys)
      (let ((from-key (if (listp entry) (car entry) entry))
	    (to-key   (if (listp entry) (cdr entry) entry)))
	(define-key new-map to-key (lookup-key from-map from-key))))
     new-map))

(defun sj/replace-cmd-in-map (map old new)
  "Replace all occurences of command OLD in keymap MAP with command NEW."
  (dolist (keyseq (where-is-internal old map))
    (define-key map keyseq new)))

(defun sj/elisp-electric-delete (&optional arg)
    "Deletes all preceding whitespace.
If however an ARG is supplied,  or point is inside a literal then
a normal delete is carried out"
    (interactive "P")
    (if arg
	;; do nothing special
	(backward-delete-char-untabify (prefix-numeric-value arg))
      (let ((here (point)))
	(skip-chars-backward " \t\n")
	(if (/= (point) here)
	    (delete-region (point) here)
	  (backward-delete-char-untabify 1)))))

(defun sj/elisp-electric-close-paren ()
  "Insert close parenthesis, go to the next line and indent."
  (interactive)
  (and (memq (preceding-char) '(?\  ?\t ?\n))
       (sj/elisp-electric-delete))
  (insert ?\))
  (newline-and-indent))

;; Add hackery to allow for auto-compiling of .el files on save
(defvar sj/recompile-file nil
  "A non-nil value will force a byte-recompilation on save of a buffer.
This variable is buffer-local.")
(make-variable-buffer-local 'sj/recompile-file)

(defun sj/byte-compile-file (&optional file force)
  "Byte-compile `file' if sj/recompile-file or `force' is non-nil.
`file' defaults to (buffer-file-name).
Can be run from after-save-hook."
  (when (or force sj/recompile-file)
    (byte-compile-file (or file (buffer-file-name)))))
(when user-sj-p
  (add-hook 'after-save-hook 'sj/byte-compile-file))

;; misc funcs for locating and byte-compiling newer libraries
(defun sj/find-newer-libraries (paths)
  "Return list of libraries from dirs in PATHS that are newer than their
compiled version. Also lists uncompiled libraries."
  (delq nil
	(apply 'nconc
	       (mapcar
		(lambda (dir)
		    (mapcar
		     (lambda (file)
		       ;; note: this handles missing .elc files correctly!
		       (when (file-newer-than-file-p file (concat file "c"))
			 file))
		     (directory-files dir 'full "\\.el$" 'no-sort)))
		paths))))

(defun sj/recompile-newer-libs (&optional prefix)
  "Re-byte-compile all libs with newer source in load-path that have paths
beginning with PREFIX. Returns alist of (FILE . ERROR) for libs that didn't
compile."
  (interactive)
  (let ((prefix (concat "^" (expand-file-name (or prefix sj/emacs-base-dir))))
	(compiled nil)
	(errors nil))
    (labels ((trim (prefix lib)
		   (replace-regexp-in-string prefix "..." lib nil t)))
      (dolist (lib (sj/find-newer-libraries load-path))
	(condition-case err
	    (when (string-match-p prefix lib)
	      (load-file lib)
	      (sj/byte-compile-file lib 'force)
	      (push (trim prefix lib) compiled))
	  (error (push (cons (trim prefix lib) err) errors)))))
    (message (cond ((or errors compiled)
		    (concat
		     (if errors   (format "Failed: %s\n" errors) "")
		     (if compiled (format "Compiled: %s" compiled) "")))
		   (t "All libraries are up to date!")))))

(defun sj/load-and-byte-compile-library (library)
  "Byte-compile a library after first locating it using load-path.
Loads the library file first."
  (interactive "sLibrary name: ")
  (let ((file
	 (locate-library
	  (concat library
		  (unless (equal (substring library -3) ".el") ".el")))))
    (and file
	 (load-library file)
	 (byte-compile-file file))))

;;; Tiling Emacs frames

(defvar sj/frame-rotate-direction 'left
  "The direction in which sj/tile-or-rotate-frames rotates.
Rotates to the left unless this is set to 'right.")

(setq sj/cached-frame-list nil) ; caches frame list between invocations

(defun sj/rotated-frame-list (direction use-cache-p)
  (let ((frames (if (and use-cache-p sj/cached-frame-list)
		    sj/cached-frame-list
		  (delete-dups (append (visible-frame-list) (frame-list))))))
    (sj/rotate-list frames 1 direction)))

;; The tiler.  It lays out frames with an even gap between them.
;; Repeated invocations lead to rotation.  Most of the complexity here
;; is from dealing with more frames than can fit on one screen: we lay
;; those out recursively in layers below each other.
(defun sj/tile-or-rotate-frames (&optional frames layer)
  "Tile visible frames horizontally. Repeat to rotate the tiled frames.
The direction of rotation is controlled by `sj/frame-rotate-direction'.
Changes focus to left- or rightmost frame when rotating, again controlled
by `sj/frame-rotate-direction'."
  (interactive)
  (let*
      ((repeat-p (eq last-command 'sj/tile-or-rotate-frames))
       (frames (or frames
		   (sj/rotated-frame-list sj/frame-rotate-direction repeat-p)))
       ;; select as many frames as will fit on the screen for this layer
       (frame-span (let ((total 0))
		     (sj/span-list (lambda (f)
				     (< (incf total (frame-pixel-width f))
					(x-display-pixel-width)))
				   frames)))
       (selected-frames  (car frame-span))
       (remaining-frames (cdr frame-span))
       (total-pixel-width (reduce (lambda (accum f)
				    (+ accum (frame-pixel-width f)))
				  selected-frames :initial-value 0))
       (layer (or layer 0))
       (gap (/ (- (x-display-pixel-width) total-pixel-width)
	       (+ (length selected-frames) 1)))
       ;; alternate the leftmost position between layers
       (next-x (if (zerop (% layer 2)) gap 0)))
    ;; lay out lower layers before this one
    (when remaining-frames
      (sj/tile-or-rotate-frames remaining-frames (+ layer 1)))
    (unless (zerop (% layer 2))
      ;; this makes for a more intuitive, "closed loop" rotation
      (setq selected-frames (reverse selected-frames)))
    (dolist (frame selected-frames)
      (set-frame-position frame next-x 0)
      (setq next-x (+ next-x (frame-pixel-width frame) gap))
      (when (= layer 0)
	(raise-frame frame)))
    (when (= layer 0)
      (setq sj/cached-frame-list frames)
      (select-frame-set-input-focus (case sj/frame-rotate-direction
				      ('right (car (last selected-frames)))
				      (t      (car selected-frames)))))))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
