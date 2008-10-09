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
  "A non-nil value will force a byte-recompilation on save.
This variable is buffer-local.")
(make-variable-buffer-local 'sj/recompile-file)
(defun sj/recompile-file ()
  "Byte-compile file if (buffer-local) sj/recompile-file is t.
Should be run from after-save-hook."
  (if sj/recompile-file
      (byte-compile-file (buffer-file-name))))
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


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
