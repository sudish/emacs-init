;;; sj-system-darwin --- OS X specific settings
;;
;; Copyright: Sudish Joseph
;; Created: 2003-08-03

;; Change the default font for all frames
(push '(font . "-*-Monaco-*-120-*") default-frame-alist)

(setq ns-antialias-text t
      ns-option-modifier 'meta)

(define-key global-map [ns-drag-file] 'ns-find-file)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
