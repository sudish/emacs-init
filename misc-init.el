;;; misc-init --- miscellaneous utils   [sj--95/11/06]
;;;

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

;; Add hackery to allow for auto-compiling of .el files on save
(defvar sj/recompile-file nil
  "A non-nil value will force a byte-recompilation on save of a buffer.
This variable is buffer-local.")
(make-variable-buffer-local 'sj/recompile-file)
(defconst sj/suppressed-byte-compile-warnings '(not cl-functions)
  "List of byte compiler warnings to enable or disable for personal code.
This list is set as the value of byte-compile-warnings in any file where 
sj/recompile-file is non-nil.")
(defun sj/recompile-file ()
  "Byte-compile file if (buffer-local) sj/recompile-file is t.
Should be run from after-save-hook."
  (when sj/recompile-file
    (let ((byte-compile-warnings sj/suppressed-byte-compile-warnings))
      (byte-compile-file (buffer-file-name)))))
(when user-sj-p
  (add-hook 'after-save-hook 'sj/recompile-file))

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
  (let* ((prefix (concat "^" (expand-file-name (or prefix sj/emacs-base-dir))))
	 (errors
	  (remove-if
	   (lambda (elt) (not (consp elt)))
	   (mapcar
	    (lambda (lib)
	      (condition-case err
		  (when (string-match-p prefix lib)
		    (load-file lib)
		    (byte-compile-file lib))
		(error (cons lib err))))
	    (sj/find-newer-libraries load-path)))))
    (when errors
      (message "Didn't compile: %s" errors))))

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


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
