;;; sj-init --- main init file   [sj--95/06/11]
;;;

(require 'cl)

(defconst user-sj-p (string-match "^\\(sj\\|sudish\\|joseph\\)$"
				(user-login-name))
  "Non-nil iff the user is me.")

;; custom
(setq custom-file "~/.custom")
(load "~/.custom")

;; viper
(require 'viper-util)
(require 'loadhist)
(setq viper-mode t)
(require 'viper)

;; System-specific init
(cond ((eq system-type 'darwin)
       (load "darwin-init")))

;; Quo vadis?
(setq user-mail-address "sudish@gmail.com")
(defvar user-name-string
  (concat (capitalize (user-full-name)) " <" user-mail-address ">")
  "User's fullname & email address \"fullname <email address>\"")

;; Info search path
(setq Info-additional-directory-list
      (delq nil
	    (mapcar #'(lambda (dir)
			    (cond ((file-directory-p dir) dir)
				  (t nil)))
		    '("/opt/local/share/info"
		      "/sw/share/info"
		      "/Developer/usr/share/info"
		      "/usr/share/info"
		      "/usr/info"))))

;; load misc. functions we need later
(load "misc-init")

;; Take off some of the training wheels.
(when user-sj-p
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'eval-expression 'disabled nil))

(setq default-major-mode 'indented-text-mode)
(add-hook 'text-mode-hook
	  (defun sj/text-mode-hook ()
	    (auto-fill-mode t)
	    (setq adaptive-fill-mode t)))

;; Tweak some of the defaults
(setq help-window-select        'always
      track-eol 		t
      scroll-step 		1
      scroll-conservatively     100
      next-screen-context-lines 2
      visible-bell		t
      column-number-mode	t
      require-final-newline 	'ask
      signal-error-on-buffer-boundary nil
      kill-whole-line 		t
      mouse-yank-at-point	t
      comint-scroll-to-bottom-on-output t
      enable-recursive-minibuffers 	t
      enable-local-eval                 'ask)

(setq query-replace-interactive nil
      query-replace-show-replacement t
      lazy-highlight-initial-delay 0)

(setq backup-by-copying 		nil
      backup-by-copying-when-linked 	t
      backup-by-copying-when-mismatch 	t
      backup-directory-alist 		'(("." . "~/.emacs.d/backups"))
      delete-old-versions 		t
      kept-new-versions 		6
      kept-old-versions 		2
      version-control 			t)

(setq default-truncate-lines nil
      next-line-add-newlines nil)

(setq default-indicate-empty-lines nil
      default-indicate-buffer-boundaries '((top . left) (bottom . left)))

;; When to create new frames
(setq display-buffer-reuse-frames t)
(when nil ;window-system
  (setq special-display-buffer-names
	(let ((temp-frame-params '((minibuffer) (unsplittable . t)
				   (left-fringe . 0) (right-fringe . 0))))
	  (mapcar #'(lambda (buf) (cons buf temp-frame-params))
		  '("*Help*")))))

;; garbage collection settings
(setq gc-cons-threshold (max gc-cons-threshold (* 4 1024 1024)))

;; better autosave, in a fixed directory
(setq auto-save-default  t
      auto-save-interval 300)

(require 'font-lock)
(make-face-bold 'modeline)

;; various packages
(load "lang-init")
(load "mode-init")
(load "anything-init")
(load "keymap-init")
(load "mail-init")

;; gnuserv
(setenv "GNUSERV_SHOW_EMACS" "1")        ; always raise Emacs window
(when (memq system-type '(darwin))
  (server-start))

;; filecache: caches names of files for use from minibuffer.
;; initialize the cache as the very last thing we do, once load-path is
;; fully initialized
(file-cache-add-directory-list load-path)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
