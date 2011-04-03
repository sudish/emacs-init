;;; sj-anything-init --- Initialize the anything.el package
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 2008-12-15

;; Load anything
(sj/load-path-prepend "external/anything-config")
(require 'anything)
(require 'anything-config)

;; Use anything-complete to load files, change buffers, etc.
(sj/load-path-prepend "external/anything-config/extensions")
(require 'anything-show-completion)
;; (require 'anything-complete)
;; (ido-mode -1)
;; (anything-read-string-mode t)
;; (defalias 'read-file-name (symbol-function 'anything-old-read-file-name))

;; Most of these are defined in anything-config.el, loaded below.
(defconst sj/anything-file-sources
  '(anything-c-source-buffers+
    sj/anything-source-eproject-files
    anything-c-source-recentf
    sj/anything-source-project-root-files
    anything-c-source-file-cache
    ;; anything-c-source-locate
    ;; sj/anything-source-osx-spotlight
    ))

(setq anything-sources sj/anything-file-sources)

;; Other anything source clusters
(defconst sj/anything-emacs-sources
  '(anything-c-source-extended-command-history
    anything-c-source-emacs-commands
    anything-c-source-emacs-variables
    anything-c-source-emacs-functions
    anything-c-source-info-pages))
(defun sj/anything-emacs ()
  (interactive)
  (anything sj/anything-emacs-sources))

(defconst sj/anything-code-nav-sources
  '(anything-c-source-imenu
    anything-c-source-ctags))
(defun sj/anything-code-nav ()
  (interactive)
  (anything sj/anything-code-nav-sources))

;; Files from current project root, if any
(defconst sj/anything-source-project-root-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details
                (project-root-fetch))
              (setq anything-project-root project-details)))
    (candidates . (lambda ()
                    (project-root-file-find-process anything-pattern)))
    (candidate-transformer . sj/anything-file-candidate-filter)
    (requires-pattern . 2)
    ;; Don't make me wait for project-root files, if any
    ;; (delayed)
    (volatile)
    (type . file)))

;; Files from current project root
(defconst sj/anything-source-eproject-files
  '((name . "eProject Files")
    (header-name . (lambda (name)
		     (concat name " for " sj/eproject-root)))
    (init . (lambda ()
	      (setq sj/eproject-root (and eproject-mode
					  (eproject-root))
		    sj/eproject-files (and eproject-mode
					   (eproject-list-project-files)))))
    (candidates . (lambda ()
		    (mapcar
		     (lambda (f)
		       (cons (replace-regexp-in-string sj/eproject-root "" f) f))
		     (reverse sj/eproject-files))))
    (candidate-transformer . sj/anything-file-candidate-filter)
    (type . file)))
(defun sj/anything-textmate ()
  (interactive)
  (anything sj/anything-source-eproject-files))

;; Spotlight search across home directory, if available
(defconst sj/anything-source-osx-spotlight
  '((name . "Spotlight (~)")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil
				   "mdfind" "-onlyin" (expand-file-name "~/")
				   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (candidate-transformer . sj/anything-file-candidate-filter)
    ;; (delayed)
    (volatile))
  "Source for retrieving files via Spotlight's command line utility mdfind.")

;; The default current-frame-configuration causes flickering
(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

;; Use a dedicated frame for anything.el.  Idea from HannesJanetzek:
;; http://www.emacswiki.org/cgi-bin/wiki/Anything#toc33
(setq anything-samewindow t)
(defvar sj/anything-uses-frames nil
  "Non-nil makes anything use dedicated frames for its interface.")
(defvar sj/anything-frame nil)

(defun sj/anything-initialize-frame ()
  (when (and window-system sj/anything-uses-frames)
    (unless (frame-live-p sj/anything-frame)
      (setq sj/anything-frame (make-frame '((name . "*Anything*")
					    (internal-border-width . 0)
					    (border-width . 0)))))
    (select-frame sj/anything-frame)
    (set-window-buffer (frame-selected-window sj/anything-frame)
		       (get-buffer-create anything-buffer))))

(defun sj/anything-hide-frame ()
  (when (and window-system sj/anything-uses-frames)
    (when (frame-live-p sj/anything-frame)
      ;; frame deletion lets emacs pick the next frame to use, which
      ;; is simpler and more consistent than trying to figure out what
      ;; the user wants ourselves
      (delete-frame sj/anything-frame))))

(add-hook 'anything-after-initialize-hook 'sj/anything-initialize-frame)
(add-hook 'anything-cleanup-hook 'sj/anything-hide-frame)

;; Hide boring files unless I explicitly ask for them
(defvar sj/anything-hide-boring-entries t
  "Hide entries matching the `sj/anything-boring-entries' regexp if non-nil.
This value may be toggled using `anything-sj-toggle-boring'.")

(defconst sj/anything-boring-entries
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg" ".semanticdb")
	    (or "/" eol))
       ;; Boring files
       (and (or "~" ".elc" ".DS_Store" ".class" ".la" ".o" ".pdf") eol)))
  "Entries matching this regexp will be hidden in anything buffers if
`sj/anything-hide-boring-entries' is non-nil.")

(defun sj/anything-file-candidate-filter (entries)
  "Filter out entries that match `sj/anything-boring-entries'."
  ;(message "filtering for entries %s" entries)
  (anything-c-shorten-home-path
   (if sj/anything-hide-boring-entries
       (remove-if (lambda (entry)
		    (let ((file (cond ((consp entry) (cdr entry))
				      (t entry))))
		      (string-match sj/anything-boring-entries file)))
		  entries)
     entries)))

(defun anything-sj-toggle-boring-files ()
  (interactive)
  (setq sj/anything-hide-boring-entries (not sj/anything-hide-boring-entries))
  (anything-update))

(define-key anything-map "A" 'anything-sj-toggle-boring-files)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
