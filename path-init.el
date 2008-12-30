;;; path-init.el --- Initialize load-path and exec-path
;;;

;; load-path
(defmacro sj/load-path-prepend (directory &optional info)
  "*Prepend DIRECTORY onto load-path.

DIRECTORY should be either a string or a list of strings.  In the
latter case, all elements of the list are prepended.

If DIRECTORY is relative, sj/emacs-site-dir is prepended to
DIRECTORY first.  DIRECTORY is not prepended if it is already in
load-path.  Any expansion permitted by expand-file-name,
including ~'s, is allowed.

If INFO is non-nil and a file named \"dir\" exists in DIRECTORY,
DIRECTORY is prepended to Info-additional-directory-list.  If
INFO is a string, it is treated as the location of a directory
relative to DIRECTORY in which to search for \"dir\".

Returns a list of the added directories."
  `(eval-and-compile
     (sj/load-path-prepend-1 ,directory ,info)))

(defun sj/load-path-prepend-1 (directory &optional info)
  "See sj/load-path-prepend, the preferred interface to this func."
  (cond
   ((listp directory)
    (unless (null directory)
      ;; post-order traversal to preserve list order
      (nconc (sj/load-path-prepend-1 (cdr directory) info)
	     (sj/load-path-prepend-1 (car directory) info))))
   ((stringp directory)
    (let ((d (expand-file-name directory sj/emacs-base-dir)))
      (cond ((file-directory-p d)
	     (add-to-list 'load-path d)
	     (when info
	       (let* ((info-dir (cond ((stringp info) (expand-file-name info d))
				      (t d)))
		      (info-file (concat info-dir "/dir")))
		 (when (file-readable-p info-file)
		   (add-to-list 'Info-additional-directory-list info-dir))))
	     (list d))
	    (t (signal 'args-out-of-range `(file-directory-p ,directory))))))
   (t (signal 'args-out-of-range `(stringp ,directory)))))

;; Convenience macro for paths
(defmacro sj/emacs-path (path)
  "Returns path relative to sj/emacs-base-dir."
  (if (not (eq "/" (substring path 0 1)))
      (setq path (concat "/" path)))
  (expand-file-name (concat sj/emacs-base-dir path)))

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
