;;; path-init.el --- Initialize load-path and exec-path
;;;

;; load-path
(defmacro sj/load-path-prepend (directory &optional path)
  "*Prepend DIRECTORY onto PATH.
PATH should be a symbol; if omitted, it defaults to 'load-path.

DIRECTORY should be either a string or a list of strings.  In the latter case,
all elements of the list are prepended.

If DIRECTORY is relative, sj/emacs-site-dir is prepended to DIRECTORY
first.  DIRECTORY is not prepended if it is already in PATH.  Tilde escapes
and missing trailing /'s in DIRECTORY are handled correctly."
  `(eval-and-compile
     (sj/load-path-prepend-1 ,directory ,path)))

(defun sj/load-path-prepend-1 (directory &optional path)
  "See sj/load-path-prepend."
  (cond
   ((listp directory)
    (unless (null directory)
      (sj/load-path-prepend-1 (cdr directory) path)
      (sj/load-path-prepend-1 (car directory) path)))
   ((stringp directory)
    (setq directory
	  (file-name-as-directory
	   (expand-file-name directory sj/emacs-site-dir)))
    (when (file-directory-p directory)
      (if (null path)
	  (setq path 'load-path))
      (unless (member directory (eval path))
	(push directory (symbol-value path)))
      path))
   (t
    (signal 'args-out-of-range `(stringp ,directory)))))

; make sure init and site-lisp are on load-path
(sj/load-path-prepend (list sj/emacs-init-dir sj/emacs-site-dir))

;; add various directories onto load-path
(sj/load-path-prepend '("dmacro"))

;; exec-path
(defun sj/get-shell-exec-path ()
  "Load list of directories to add to PATH from login shell.

Returns list of directories or nil otherwise.
A directory is added to the list iff it exists on this machine."
  (save-excursion
    (with-temp-buffer
      (call-process-shell-command "echo $PATH" nil t)
      (goto-char (point-min))
      (let (dirs dir)
	(while (not (eq (point) (point-max)))
	  (setq dir (buffer-substring (point)
				       (progn
					 (skip-chars-forward "^:")
					 (point))))
	  (if (and dir (file-directory-p dir))
	      (setq dirs (append (list (expand-file-name dir)) dirs)))
	  (skip-chars-forward ":"))
	(nreverse dirs)))))

;; Initialize exec-path and $PATH to a better value.
;; Some Emacs commands use your login shell whilst others use /bin/sh,
;; setting PATH here is the easiest way to communicate with these sub-shells.
(setq exec-path
      (delete-dups (nconc (sj/get-shell-exec-path) exec-path)))
(setenv "PATH" (mapconcat #'identity exec-path ":"))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
