;;; bbdb-init --- the Insidious Big Brother Database   [sj 95/06/11]
;;;

(require 'bbdb)
(and (fboundp 'bbdb-initialize) (bbdb-initialize))

(setq bbdb/gnus-header-prefer-real-names nil
      bbdb-message-marker-field 	'gnus-mark
      bbdb-default-area-code 		614
      bbdb-notice-auto-save-file 	t
      bbdb-offer-save 			'just-do-it
      bbdb-use-pop-up 			nil           ;window-system
      bbdb-pop-up-elided-display	t
      bbdb-pop-up-target-lines 		1
      bbdb-always-add-addresses 	t
      bbdb-new-nets-always-primary 	nil
      bbdb-completion-display-record 	nil)
(when user-sj-p
  (setq bbdb-user-mail-names "\\(sudish\\|sj\\)@"))

(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus t)
(add-hook 'gnus-startup-hook #'(lambda () (require 'gnus-sum)))

(add-hook 'bbdb-change-hook 'bbdb-delete-redundant-nets)
(add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
(add-hook 'bbdb-create-hook 'bbdb-creation-date-hook)

(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(when user-sj-p
  (setq bbdb-auto-notes-alist
	'(("Newsgroups"    (".*" newsgroups bbdb/gnus-update-newsgroup))
	  ("Organization"  (".*" company 0 t))))
  (setq bbdb-auto-notes-ignore
	'(("Organization" . "gateway\\|collabra server"))))
(defun bbdb/gnus-update-newsgroup (groups)
  "Returns current newsgroup, if we are reading news."
  ;; this works with (ding)
  (if gnus-newsgroup-name
      (gnus-group-real-name gnus-newsgroup-name)
    ""))

;; bbdb-print: pretty print bbdb files
(autoload 'bbdb-print "bbdb-print" "Print BBDB buffer" t)
(setq bbdb-print-file-name	(expand-file-name "~/bbdb.tex")
      bbdb-print-elide		nil
      bbdb-print-require	'name)
;(eval-after-load "bbdb" '(define-key bbdb-mode-map "P" 'bbdb-print))

;; hack a seperate window for bbdb
;; WARNING: this stuff is probably severely broken on all flavors of Emacs;
;; I haven't used it (or looked at it) since Emacs 19.29 or so.
;; Do NOT enable it unless you wish to hack lots.
;(defvar bbdb-buffer-name "*BBDB*")
;(defvar get-buffer-window-broken-packages
;  (list bbdb-buffer-name)
;  "*List of buffer names which get-buffer-window is forced to search
;for in other frames..")

;(setq special-display-buffer-names
;      (cons bbdb-buffer-name special-display-buffer-names))


;(defadvice get-buffer-window (around mms-get-buffer-window act)
;  (if (member
;       (let ((b (ad-get-arg 0))) (if (bufferp b) (buffer-name b) b))
;       get-buffer-window-broken-packages)
;      (ad-set-args 1 '(t)))
;  ad-do-it)

;(defun bbdb-setup-frame ()
;  (let ((bbdb-electric-p nil)
;	(special-display-frame-alist '((name . "BBDB")
;				       (left . -)
;				       (top . -)
;				       (height . 5)
;				       (width . 80)
;				       (unsplittable . t)
;				       (icon-type . nil)
;				       (menu-bar-lines . 0)
;				       (auto-raise . t)
;				       (vertical-scroll-bars . nil)
;				       (minibuffer . nil)
;				       (visibility . nil)
;				       )))
;    (bbdb "JUNK" nil)))

;(defvar bbdb-suspend-frame 'invisible
;  "*Specifies action for bbdb-suspend-frame to take.
;If this is the symbol `iconify', the BBDB frame will be iconified.
;Any other value will cause it to become invisible.")

;(defun bbdb-suspend-frame ()
;  "Suspend the BBDB frame.
;The actual action taken depends on the value of `bbdb-suspend-frame'."
;  (let* ((window (get-buffer-window bbdb-buffer-name t))
;	 (frame (and window (window-frame window))))
;    (and frame
;	 (cond
;	  ((and (eq bbdb-suspend-frame 'iconify)
;		(not (eq (frame-visible-p frame) 'icon)))
;	   (iconify-frame frame))
;	  ((frame-visible-p frame)
;	   (make-frame-invisible frame))))))

;;(add-hook 'bbdb-load-hook 'bbdb-setup-frame)
;;(add-hook 'win-vm-iconify-hook 'bbdb-suspend-frame)
;;(add-hook 'gnus-startup-hook 
;;	  '(lambda () 
;;	     (add-hook 'gnus-article-prepare-hook 'bbdb-suspend-frame)
;;	     (add-hook 'gnus-exit-group-hook 'bbdb-suspend-frame))
;;	  t)

;; This is so that bbdb/gnus-lines-and-from behaves in a saner manner

;; this turns off support for the BSD &-in-GECOS hack, 
;; these days, many use addresses like "Ma & Pa" <thefamily@foo.com>.
(setq mail-extr-stupid-ampersand-convention nil)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
