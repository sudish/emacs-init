;;; sj-funcs --- Various useful functions
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-11-06

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

(defun sj/replace-key-in-map (map old new)
  "Replace all occurences of command OLD in keymap MAP with command NEW."
  (mapc
   #'(lambda (keyseq)
       (define-key map keyseq new)))
  (where-is-internal old map))

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
      (mapc
       (lambda (lib)
	 (condition-case err
	     (when (string-match-p prefix lib)
	       (load-file lib)
	       (sj/byte-compile-file lib 'force)
	       (push (trim prefix lib) compiled))
	   (error (push (cons (trim prefix lib) err) errors))))
       (sj/find-newer-libraries load-path)))
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

;; Convenience functions
(macrolet ((f-param (param frame)
		    `(cdr (assoc ,param (frame-parameters ,frame)))))
  (defun sj/frame-left  (frame) (f-param 'left frame))
  (defun sj/frame-top   (frame) (f-param 'top frame))
  (defun sj/frame-height (frame) (frame-pixel-height frame)))

;; The frame comparator. It defines the tiling order.
(defun* sj/frame< (f1 f2) 		; defun* because of return-from below
  "Returns t if frame `f1' is considered \"less than\" frame `f2'.
Suitable for use as a predicate for the `sort' function."
  (loop for (getter . cmp) in '((sj/frame-left   . <)
				(sj/frame-top    . <)
				(sj/frame-height . >))
	do
	(let ((f1-val (funcall getter f1))
	      (f2-val (funcall getter f2)))
	  (when (/= f1-val f2-val)
	    ;; Short-circuit and exit the func as soon as we have an
	    ;; ordering between the two frames.
	    (return-from sj/frame< (funcall cmp f1-val f2-val)))))
  ;; Return f1 < f2 if they're equal in all respects
  t)

;; The tiler.  It lays out frames with an even gap between them.
;; Doesn't deal gracefully with more frames than can fit on the
;; display.
(defun sj/tile-frames ()
  "Tile visible frames horizontally."
  (interactive)
  (let* ((frames (sort (filtered-frame-list
			(lambda (f) (frame-visible-p f))) #'sj/frame<))
	 (num-frames (length frames))
	 (total-pixel-width (cond ((> num-frames 1)
				   (reduce (lambda (f1 f2)
					     (+ (cond ((framep f1)
						       (frame-pixel-width f1))
						      (t f1))
						(frame-pixel-width f2)))
					   frames))
				  (t (frame-pixel-width (car frames)))))
	 (gap (/ (- (x-display-pixel-width) total-pixel-width)
		 (+ num-frames 1)))
	 (next-x gap))
    (dolist (frame frames)
      (set-frame-position frame next-x 0)
      (setq next-x (+ next-x (frame-pixel-width frame) gap))
      (make-frame-visible frame))))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
