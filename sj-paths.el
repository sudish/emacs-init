;;; sj-paths --- Initialize emacs paths (load-path, exec-path, ...)
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

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

;; Get environment variables from the login shell
(defun sj/get-shell-env-var (var &optional precmd)
  "Get the value of an environment variable from the login shell.
If optional string `precmd' is provided, it is executed in the subshell first."
  (save-excursion
    (with-temp-buffer
      (let ((cmd (concat (or precmd "true") " >/dev/null 2>&1; "
			 (format "echo $%s" var))))
	(call-process-shell-command cmd nil t)
	(replace-regexp-in-string "\r?\n\\'" "" (buffer-string))))))

;; Initialize Emacs exec-path and sub-shell $PATH to the value set in
;; my login shell.
;;
;; Some Emacs commands use the login shell whilst others use /bin/sh.
;; Setting PATH in the Emacs environment is the easiest way to set it
;; correctly for these sub-shells.
(setq exec-path
      (delete-dups 
       (nconc (split-string (sj/get-shell-env-var "PATH") ":" t) exec-path)))
(setenv "PATH" (mapconcat #'identity exec-path ":"))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
