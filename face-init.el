;;; face-init --- all face/font-related stuff    [sj 95/06/11]
;;;

;; This has been heavily reworked from hilit-lookup-face-create, all
;; it retains is the interface and some of the ideas.
(defun sj/hilit-lookup-face-create (face)
  "Get a FACE, or create it if it doesn't exist.  In order for it to
properly create the face, the following naming convention must be used:
    [reverse-](fgcolor[/bgcolor])[-bold][-italic][-underline]

Example: (hilit-lookup-face-create 'red-bold) returns a face named
red-bold that is red and bold.

Each color is either the name of an X color (see .../X11/lib/X11/rgb.txt),
a hexadecimal specification of the form \"hex-[0-9A-Fa-f]+\", or \"default\"."
  ;; Since the face-name encodes the face property, don't do all this
  ;; work if a face of the same name already exists -- it must have
  ;; the same properties.
  (unless (facep face)
    (save-excursion
      (let (reverse-p fgcolor bgcolor bold-p italic-p underline-p)
	(set-buffer (get-buffer-create " *sj/face temp*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(setq case-fold-search t)
	(insert (symbol-name face))
	(goto-char 0)

	;; Parse face specs -- we cannot modify the new face until
	;; we've set it's parent face (see below).
	
	(when (looking-at "^reverse-?")
	  (setq reverse-p t)
	  (replace-match "" t t))

	(if (looking-at "\\(hex-\\)?\\([A-Za-z0-9]+\\)")
	    (progn
	      (setq fgcolor (concat (and (match-beginning 1) "#")
				    (match-string 2)))
	      (replace-match "" t t)
	      (when (looking-at "/\\(hex-\\)?\\([A-Za-z0-9]+\\)")
		(setq bgcolor (concat (and (match-beginning 1) "#")
				      (match-string 2)))
		(replace-match "" t t)))
	  (kill-buffer (current-buffer))
	  (error "bad face name %S" face))
	
	;; Special-case the color "default" to mean "use the default color."
	(and (string= "default" fgcolor) (setq fgcolor nil))
	(and (string= "default" bgcolor) (setq bgcolor nil))

	;; Specifying a font-property twice is allowed, nor do we care
	;; about the order in which font-properties are specified.
	(while (not (eobp))
	  (cond ((looking-at "-bold")      (setq bold-p t))
		((looking-at "-italic")    (setq italic-p t))
		((looking-at "-underline") (setq underline-p t))
		(t
		 (kill-buffer (current-buffer))
		 (error "bad face name %S" face)))
	  (replace-match "" t t))

	;; Create new face and set properties.
	;;
	;; We handle combinations of the bold and italic font
	;; properties by inheriting from an appropriate base face.
	;; This lets us override font properties on a per-locale basis
	;; by modifying the base face as needed.
	;;
	;; For e.g., we might prefer using a font of a different size
	;; for frames opened on a display of a different size than our
	;; default display.  Using inheritance lets us resize all our
	;; fonts by resizing just the 4 base fonts.  (And we can do so
	;; only in the locale of the new display, so that the original
	;; display retains the sizes of all its fonts -- XEmacs
	;; specifiers rule!)
	(make-face face)
	(and bold-p      (make-face-bold face))
	(and italic-p    (make-face-italic face))
	(and fgcolor     (set-face-foreground face fgcolor))
	(and bgcolor     (set-face-background face bgcolor))
	(and reverse-p   (invert-face face))
	(and underline-p (set-face-underline-p face t))
	
	(copy-face face face))
      (kill-buffer (current-buffer))))
  (check-face face))

(defun sj/set-face (face spec &optional no-symbol)
  "Create face FACE according to SPEC.
FACE will be both a face-name and a symbol with value SPEC.
SPEC will be a variable with value SPEC, as well as being a face.
If NO-SYMBOL is non-nil, FACE will simply be a face.

This twisted definition lets you use FACE safely in every package."
  ;; first we make both FACE and SPEC into the same face
  (let ((f (sj/hilit-lookup-face-create spec)))
    (and f (copy-face f face))
    (or no-symbol
	;; then both get the value of SPEC
	(progn
	  (set face spec)
	  (set spec spec)))))

(defun sj/set-faces (specs)
  (loop for (face . spec) in specs
	do (sj/set-face face spec)))

(provide 'face-init)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
