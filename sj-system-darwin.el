;;; sj-system-darwin --- OS X specific settings
;;
;; Copyright: Sudish Joseph
;; Created: 2003-08-03

;; Default and inital frame parameters
(defconst sj/default-frame-parameters '((width . 80) (height . 50)
					(font . "-*-Monaco-*-120-*"))
  "Default frame parameters for all frames, including the initial one.")
(let ((left-pos (/ (- (x-display-pixel-width) (frame-pixel-width)) 2)))
  (setq initial-frame-alist (append `((top . 0) (left . ,left-pos))
				    sj/default-frame-parameters
				    initial-frame-alist)
	default-frame-alist (append sj/default-frame-parameters
				    default-frame-alist)))

(setq ns-antialias-text t
      ns-option-modifier 'meta)

(define-key global-map [ns-drag-file] 'ns-find-file)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
