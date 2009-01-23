;;; anything-init --- initialize the anything package   [sj--08/12/15]

;; Don't make me wait for project-root files, if any
(setq project-root-anything-config-files 
      (delete '(delayed) project-root-anything-config-files))

;; default of current-frame-configuration causes flickering
(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

;; Initialize anything-sources before loading 'anything to prevent it from
;; pre-loading the content for sources we're not interested in
(setq sj/anything-sources
      '(anything-c-source-imenu
	project-root-anything-config-files
	project-root-anything-config-bookmarks
	anything-c-source-recentf
	anything-c-source-locate))
(setq anything-sources sj/anything-sources)
(require 'anything)
(sj/load-path-prepend "external/anything-config")
(require 'anything-config)
(setq anything-sources sj/anything-sources) ; override anything-config


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
