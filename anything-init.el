;;; anything-init --- initialize the anything package   [sj--08/12/15]

;; Don't make me wait for project-root files, if any
(setq project-root-anything-config-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details
                (project-root-fetch))
              (setq anything-project-root project-details)))
    (candidates . (lambda ()
                    (project-root-file-find-process anything-pattern)))
    (candidate-transformer . project-root-anything-colourfy-hits)
    ;; (requires-pattern . 2)
    ;; (delayed)
    (volatile)
    (type . file)))
;; (setq project-root-anything-config-files
;;       (delete '(volatile)
;; 	      (delete '(delayed) project-root-anything-config-files)))

;; delay the recentf list, it tends to show up first always as it's precomputed
(setq anything-c-source-recentf
      '((name . "Recentf")
	(candidates . recentf-list)
	(match . (anything-c-match-on-file-name
		  anything-c-match-on-directory-name))
	(delayed)
	(type . file)))

;; default of current-frame-configuration causes flickering
(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

;; Initialize anything-sources before loading 'anything to prevent it from
;; pre-loading the content for sources we're not interested in
(setq sj/anything-sources
      '(;anything-c-source-imenu
	project-root-anything-config-files
	project-root-anything-config-bookmarks
	anything-c-source-recentf
	anything-c-source-locate))
;(setq anything-sources sj/anything-sources)
(require 'anything)
(sj/load-path-prepend "external/anything-config")
(require 'anything-config)
(setq anything-sources sj/anything-sources) ; override anything-config

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


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
