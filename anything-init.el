;;; anything-init --- initialize the anything package   [sj--08/12/15]

;; imenu support from http://www.emacswiki.org/emacs/AnythingSources
(defvar anything-c-imenu-delimiter "/")
(defvar anything-c-cached-imenu-alist nil)
(defvar anything-c-cached-imenu-candidates nil)
(defvar anything-c-cached-imenu-tick nil)
(make-variable-buffer-local 'anything-c-cached-imenu-alist)
(make-variable-buffer-local 'anything-c-cached-imenu-candidates)
(make-variable-buffer-local 'anything-c-cached-imenu-tick)

;; pulled up from anything-c-source-menu defn below
(defun sj/anything-c-source-imenu-candidates ()
  (with-current-buffer anything-c-imenu-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq anything-c-cached-imenu-tick tick)
	  anything-c-cached-imenu-candidates
	(setq anything-c-cached-imenu-tick tick
	      anything-c-cached-imenu-candidates
	      (condition-case nil
		  (mapcan
		   #'(lambda (entry)
		       (if (listp (cdr entry))
			   (mapcar
			    #'(lambda (sub)
				(concat (car entry)
					anything-c-imenu-delimiter
					(car sub)))
			    (cdr entry))
			 (list (car entry))))
		   (setq anything-c-cached-imenu-alist
			 (imenu--make-index-alist)))
		(error nil)))))))
(defun sj/anything-c-source-imenu-action (entry)
  (let* ((pair (split-string entry anything-c-imenu-delimiter))
	 (first (car pair))
	 (second (cadr pair)))
    (imenu
     (if second
	 (assoc second 
		(cdr (assoc first anything-c-cached-imenu-alist)))
       (assoc entry anything-c-cached-imenu-alist)))))
(setq anything-c-source-imenu
      '((name . "Imenu")
	(init . (lambda ()
		  (setq anything-c-imenu-current-buffer
			(current-buffer))))
	(candidates . sj/anything-c-source-imenu-candidates)
	(volatile)
	(action . sj/anything-c-source-imenu-action)))

;; project-root integration from http://www.shellarchive.co.uk/
(defun anything-project-root-find-files (pattern)
  (when anything-project-root
      (start-process-shell-command "project-root-find"
                                   nil
                                   "find"
                                   anything-project-root
                                   (find-to-string
                                    `(and (prune (name "*.svn" "*.git"))
                                          (name ,(concat "*" pattern "*"))
                                          (type "f"))))))
(defvar anything-c-source-project-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details (project-root-fetch))
              (setq anything-project-root (cdr project-details))))
    (candidates . (lambda ()
                    (anything-project-root-find-files anything-pattern)))
    (type . file)
    (requires-pattern . 2)
    (volatile)
    (delayed)))

;; anything
(require 'anything)
(sj/load-path-prepend (sj/emacs-path 'ext "anything-config"))
(require 'anything-config)
(setq anything-sources
      '(anything-c-source-file-name-history
	anything-c-source-imenu
	anything-c-source-buffers
	anything-c-source-buffer-not-found
	anything-c-source-project-files
	project-root-anything-config-bookmarks
	project-root-anything-config-files
	anything-c-source-locate))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
