;;; sj-system-darwin --- OS X specific settings
;;
;; Copyright: Sudish Joseph
;; Created: 2003-08-03

(prefer-coding-system 'utf-8)

;; Monospaced fonts from excellent review at:
;; http://hivelogic.com/articles/view/top-10-programming-fonts
(defconst sj/default-font
  "-apple-Inconsolata-medium-r-normal--14-*-*-*-*-*-iso10646-1"
  ;; "-apple-Consolas-medium-r-normal--13-*-*-*-*-*-iso10646-1"
  ;; "-apple-Droid Sans Mono-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-apple-DejaVu Sans Mono-medium-r-normal--13-*-*-*-*-*-iso10646-1"
  ;; "-apple-Andale Mono-medium-r-normal--13-*-*-*-*-*-iso10646-1"
  ;; "-apple-Monaco-medium-r-normal--12-*-*-*-*-*-iso10646-1"
    "Default font under OS X. Stick to Unicode if possible (iso10646-1).")

;; Default and inital frame parameters
(defconst sj/default-frame-parameters `((width . 80) (height . 50)
					(font . ,sj/default-font))
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
