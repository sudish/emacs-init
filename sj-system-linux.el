;;; sj-system-linux --- Linux specific settings
;;
;; Copyright: Sudish Joseph
;; Created: 2003-08-03

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Monospaced fonts from excellent review at:
;; http://hivelogic.com/articles/view/top-10-programming-fonts
(defconst sj/default-font
  "-*-Inconsolata-medium-r-normal--14-*-*-*-*-*-iso10646-1"
  ;; "-*-Menlo-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-Monaco-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-Consolas-medium-r-normal--14-*-*-*-*-*-iso10646-1"
  ;; "-*-Droid Sans Mono-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-DejaVu Sans Mono-medium-r-normal--13-*-*-*-*-*-iso10646-1"
  ;; "-*-Liberation Mono-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-Andale Mono-medium-r-normal--13-*-*-*-*-*-iso10646-1"
  "Default font. Stick to Unicode if possible (iso10646-1).")

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

(global-set-key (kbd "<XF86Forward>") 'other-window)
(global-set-key (kbd "<XF86Back>") 'previous-buffer)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
