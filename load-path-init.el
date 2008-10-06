;;; load-path-init.el --- add packages to load-path
;;;

;; some handy stuff
(defvar load-path-prepend-prefix "~/gnuemacs/site-lisp/"
  "Default prefix path to be prepended to a path.  Used by load-path-prepend")
(setq load-path-prepend-prefix
      (file-name-as-directory (expand-file-name load-path-prepend-prefix)))

(defun load-path-prepend (directory &optional path)
  "*Prepend DIRECTORY onto PATH. 
PATH should be a symbol; if omitted, it defaults to 'load-path.

DIRECTORY should be either a string or a list of strings.  In the latter case,
all elements of the list are prepended.

If DIRECTORY is relative, load-path-prepend-prefix is prepended to DIRECTORY
first.  DIRECTORY is not prepended if it is already in PATH.  Tilde escapes
and missing trailing /'s in DIRECTORY are handled correctly."
  (interactive "DDirectory to add: ")
  (cond 
   ((listp directory)
    (unless (null directory)
      (load-path-prepend (cdr directory) path)
      (load-path-prepend (car directory) path)))
   ((stringp directory)
    (setq directory 
	  (file-name-as-directory
	   (expand-file-name directory load-path-prepend-prefix)))
    (when (file-directory-p directory)
      (if (null path)
	  (setq path 'load-path))
      (unless (member directory (eval path)) 
	(push directory (symbol-value path)))
      path))
   (t
    (signal 'args-out-of-range `(stringp ,directory)))))

; make sure init and site-list are on load-path
(load-path-prepend '("~/gnuemacs/init" "~/gnuemacs/site-lisp"))

;; add various directories onto load-path
(load-path-prepend '("dmacro"))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
