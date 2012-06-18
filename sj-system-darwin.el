;;; sj-system-darwin --- OS X specific settings
;;
;; Copyright: Sudish Joseph
;; Created: 2003-08-03

(setq sj/osx-version
      (let ((version
	     (progn
	       (string-match ".*-darwin\\(.+\\)$" system-configuration)
	       (match-string 1 system-configuration))))
	(cond
	 ((not (version< version "11")) 'lion)
	 ((not (version< version "10")) 'snow-leopard)
	 ((not (version< version "9"))  'leopard)
	 (t 'old))))

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Monospaced fonts from excellent review at:
;; http://hivelogic.com/articles/view/top-10-programming-fonts
(defconst sj/default-font
  ;; "-*-Inconsolata-medium-r-normal--14-*-*-*-*-*-iso10646-1"
  "-*-menlo-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-Menlo-bold-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-Monaco-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-Consolas-medium-r-normal--14-*-*-*-*-*-iso10646-1"
  ;; "-*-Droid Sans Mono-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-DejaVu Sans Mono-medium-r-normal--13-*-*-*-*-*-iso10646-1"
  ;; "-*-Liberation Mono-medium-r-normal--12-*-*-*-*-*-iso10646-1"
  ;; "-*-Andale Mono-medium-r-normal--13-*-*-*-*-*-iso10646-1"
  "Default font under OS X. Stick to Unicode if possible (iso10646-1).")

;; Default and inital frame parameters
(defconst sj/default-frame-parameters `((width . 80) (height . 50)
					(font . ,sj/default-font))
  "Default frame parameters for all frames, including the initial one.")

(when window-system
  (setq initial-frame-alist (append '((top . 0))
				    sj/default-frame-parameters
				    initial-frame-alist)
	default-frame-alist (append sj/default-frame-parameters
				    default-frame-alist)))

;; Typester's fullscreen hack
(when (fboundp 'ns-toggle-fullscreen)
  (define-key global-map [(super return)] 'ns-toggle-fullscreen))

;; Emacs HEAD now has support for auto-hiding the OS X menubar.
;; Combine this with moving the frame decoration offscreen and
;; we have fullscreen mode under OS X.
;; (see e7f047d6f3e1ea53c8469c28279c2c284fd4d655)
(when (boundp 'ns-auto-hide-menu-bar)
  (define-key global-map [(super S-return)] 'sj/ns-toggle-menu-bar)
  (defun sj/ns-toggle-menu-bar ()
    "Toggle the auto-hide of the menu bar."
    (interactive)
    (setq ns-auto-hide-menu-bar (not ns-auto-hide-menu-bar)))
  (define-key global-map [(super return)] 'sj/ns-make-frame-fullscreen)
  (defun sj/ns-make-frame-fullscreen ()
    "Make the current frame fullscreen."
    (interactive)
    (setq ns-auto-hide-menu-bar t)
    (labels ((fp (p) (frame-parameter (selected-frame) p)))
      ;; Make this frame large enough to cover the whole screen.
      ;; set-frame-size takes character rows and columns, so convert
      ;; all the pixel-based values accordingly.
      (let* ((rows (/ (display-pixel-height) (frame-char-height)))
	     (cols (/ (display-pixel-width)  (frame-char-width)))
	     (fringe-pixels (+ (fp 'left-fringe) (fp 'right-fringe)))
	     (fringe (/ fringe-pixels (frame-char-width)))
	     (scrollbar (/ (fp 'scroll-bar-width) (frame-char-width)))
	     (real-cols (- cols fringe scrollbar)))
	(set-frame-size (selected-frame) real-cols rows))
      ;; Move this frame's window decoration offscreen.
      ;; - The magic number here is the size of the decorator in pixels.
      ;; - vertical-gap is any leftover vertical space (pixels) after
      ;;   computing the number of rows above. We distribute this evenly
      ;;   at the top and bottom.
      ;; - vertical-offset must be negative to move the window decoration
      ;;   offscreen.
      (let* ((decorator-size 24)
	     (vertical-gap (mod (display-pixel-height) (frame-char-height)))
	     (vertical-offset (- (/ vertical-gap 2) decorator-size)))
	(set-frame-position (selected-frame) 0 vertical-offset)))))

(when (boundp 'mac-carbon-version-string)
  (defun sj/mac-carbon-toggle-frame-fullscreen ()
    "Make the current frame fullscreen."
    (interactive)
    (let* ((frame (selected-frame))
	   (fs-param (if (eq (frame-parameter frame 'fullscreen) 'fullboth)
			 nil
		       'fullboth)))
      (set-frame-parameter frame 'fullscreen fs-param)))
  (define-key global-map [(super return)] 'sj/mac-carbon-toggle-frame-fullscreen))

;; Distinguish between various Emacs ports to OS X
(cond
 ;; ns port
 ((boundp 'ns-version-string)
  (setq ns-antialias-text t
	ns-option-modifier 'meta)
  (define-key global-map [ns-drag-file] 'ns-find-file))
 ;; mac port
 ((boundp 'mac-carbon-version-string)
  (setq mac-command-modifier 'super
	mac-option-modifier  'meta)
  ;; Command-S to save, C to copy, V to paste, etc.
  (let ((keys '(("\C-x\C-s"    . [(super s)])
		("\C-w"        . [(super x)])
		("\M-w"        . [(super c)])
		("\C-y"        . [(super v)])
		("\C-xh"       . [(super a)])
		([(control /)] . [(super z)]))))
    (sj/copy-keys-from-keymap global-map keys global-map))))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
