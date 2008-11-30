;;; path-init.el --- Initialize load-path and exec-path
;;;

;; load-path
(defvar sj/load-path-prepend-prefix "~/gnuemacs/site-lisp/"
  "Default prefix path to be prepended.  Used by sj/load-path-prepend")
(setq sj/load-path-prepend-prefix
      (file-name-as-directory (expand-file-name sj/load-path-prepend-prefix)))

(defun sj/load-path-prepend (directory &optional path)
  "*Prepend DIRECTORY onto PATH.
PATH should be a symbol; if omitted, it defaults to 'load-path.

DIRECTORY should be either a string or a list of strings.  In the latter case,
all elements of the list are prepended.

If DIRECTORY is relative, sj/load-path-prepend-prefix is prepended to DIRECTORY
first.  DIRECTORY is not prepended if it is already in PATH.  Tilde escapes
and missing trailing /'s in DIRECTORY are handled correctly."
  (interactive "DDirectory to add: ")
  (cond
   ((listp directory)
    (unless (null directory)
      (sj/load-path-prepend (cdr directory) path)
      (sj/load-path-prepend (car directory) path)))
   ((stringp directory)
    (setq directory
	  (file-name-as-directory
	   (expand-file-name directory sj/load-path-prepend-prefix)))
    (when (file-directory-p directory)
      (if (null path)
	  (setq path 'load-path))
      (unless (member directory (eval path))
	(push directory (symbol-value path)))
      path))
   (t
    (signal 'args-out-of-range `(stringp ,directory)))))

; make sure init and site-list are on load-path
(sj/load-path-prepend '("~/gnuemacs/init" "~/gnuemacs/site-lisp"))

;; add various directories onto load-path
(sj/load-path-prepend '("dmacro"))

;; exec-path
(defvar sj/exec-path-config-file "~/.sj-config/PATH"
  "File containing list of directories to add to exec-path.")
(setq sj/exec-path-config-file (expand-file-name sj/exec-path-config-file))

(defun sj/load-exec-path-config-file ()
  "Load list of directories to add to PATH from sj/load-exec-path-config-file.

Returns list of directories or nil if the file does not exist or is empty.
A directory is added to the list iff it exists."
  (when (file-readable-p sj/exec-path-config-file)
    (save-excursion
      (with-temp-buffer
	(insert-file-contents-literally sj/exec-path-config-file)
	(goto-char (point-min))
	(let (dirs file beg)
	  (while (not (eq (point) (point-max)))
	    (setq file (buffer-substring (line-beginning-position)
					    (line-end-position)))
	    (setq file (progn
			 (string-match "^\\s-*\\(\\S-+\\)\\s-*$" file)
			 (match-string 1 file)))
	    (forward-line)
	    (if (and file (file-directory-p file))
		(add-to-list 'dirs (expand-file-name file) 'append)))
	  dirs)))))

;; Initialize exec-path and $PATH to a better value.
;; Some Emacs commands use your login shell whilst others use /bin/sh,
;; setting PATH here is the easiest way to communicate with these sub-shells.
(setq exec-path
      (delete-dups (append (sj/load-exec-path-config-file) exec-path)))
(setenv "PATH" (mapconcat #'identity exec-path ":"))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
