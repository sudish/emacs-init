;;; sj-anything-init --- Initialize the anything.el package
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 2008-12-15

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
    ;; Don't make me wait for project-root files, if any
    ;; (requires-pattern . 2)
    ;; (delayed)
    (volatile)
    (type . file)))

;; Emacs recentf list
(defconst sj/anything-source-recentf
  '((name . "Recentf")
    (candidates . recentf-list)
    (match . (anything-c-match-on-file-name
	      anything-c-match-on-directory-name))
    (candidate-transformer . sj/anything-file-candidate-filter)
    (type . file)))

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

(defconst sj/anything-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (start-process "locate-process" nil
				   "locate" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (candidate-transformer . sj/anything-file-candidate-filter)
    ;; (delayed)
    (volatile))
  "Source for retrieving files matching the current input pattern
with locate.")

;; default of current-frame-configuration causes flickering
(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

;; Initialize anything-sources before loading 'anything to prevent it from
;; pre-loading the content for sources we're not interested in
(setq sj/anything-sources
      '(sj/anything-source-recentf
	sj/anything-source-project-root-files
	project-root-anything-config-bookmarks
	sj/anything-source-locate
	sj/anything-source-osx-spotlight))
(setq anything-sources sj/anything-sources)
(require 'anything)
(sj/load-path-prepend "external/anything-config")
(require 'anything-config)
;; anything-config unconditionally overwrittes anything-sources, reset it
(setq anything-sources sj/anything-sources)

;; Use a dedicated frame for anything.el
;; From HannesJanetzek: http://www.emacswiki.org/cgi-bin/wiki/Anything#toc33
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
