;; EMACS startup file

;; Always start in $HOME
(cd (expand-file-name "~"))

(defvar sj/emacs-type
  (cond ((string-match "xemacs\\|lucid" emacs-version) 'xemacs)
	(t 'emacs)))

;; Base directory for all my emacs files.
(setq sj/emacs-base-dir	(expand-file-name "~/gnuemacs"))
;; In case I run another emacs variant again.
;; (setq sj/emacs-base-dir
;;       (expand-file-name
;;        (cond
;; 	((eq sj/emacs-type 'xemacs) "~/xemacs")
;; 	((eq sj/emacs-type 'emacs)  "~/gnuemacs")
;; 	(t (error "Unknown Emacs type '%s' in ~/.emacs" sj/emacs-type)))))

;; Three directories for elisp files:
;; - init/ for my code
;; - site-lisp/ for downloaded external packages
;; - external/ for git/svn/cvs directories for external packages
(setq sj/emacs-init-dir (concat sj/emacs-base-dir "/init")
      sj/emacs-ext-dir  (concat sj/emacs-base-dir "/external")
      sj/emacs-site-dir (concat sj/emacs-base-dir "/site-lisp"))

;; Convenience path macro
(defmacro sj/emacs-path (type &optional suffix)
  "Returns emacs lisp path computed from TYPE and SUFFIX.

If TYPE is a symbol, it should be one of init, ext or site and the
corresponding directory under sj/emacs-base-dir will be used for the path.
If TYPE is a string, it will be assumed to be a subdirectory of the base dir
and concatenated with the base dir to form the path.
If TYPE is anything else, it will be evaluated to obtain a symbol.

SUFFIX, if present, is appended to the end of the path computed above."
  (if (and suffix (not (eq "/" (substring suffix 0 1))))
      (setq suffix (concat "/" suffix)))
  (let ((path (if (stringp type)
		  ;; A string is treated as a sub-dir of the base dir
		  (concat sj/emacs-base-dir
			  (if (not (eq "/" (substring type 0 1)))
			      (concat "/" type)
			    type))
		;; Else it's a symbol or something that evaluates to a symbol
		;; from: init, ext, site
		(if (not (symbolp type)) ;
		    (setq type (eval type)))
		(cond
		 ((eq type 'init) sj/emacs-init-dir)
		 ((eq type 'ext)  sj/emacs-ext-dir)
		 ((eq type 'site) sj/emacs-site-dir)
		 (t (error "Unknown path type '%s' in ~/.emacs" type))))))
    (concat path suffix)))

;; Load init.el, which bootstraps everything else
(load (sj/emacs-path 'init "init"))

;; The message below appears only on succesful loading of all the init
;; files, making it easier to notice failed loading
(defun sj/after-init-hook ()
  (message (emacs-version)))
(add-hook 'after-init-hook 'sj/after-init-hook 'append)
(setq inhibit-startup-echo-area-message "sj")
