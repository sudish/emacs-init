;;; darwin-init --- Mac OS X init   [sj--03/08/03]
;;;

;; Change the default font for all frames
(push '(font . "-*-Monaco-*-120-*") default-frame-alist)

(setq ns-antialias-text t
      ns-option-modifier 'meta)

(define-key global-map [ns-drag-file] 'ns-find-file)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
