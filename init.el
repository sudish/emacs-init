;;; init --- main init file   [sj--95/06/11]
;;;

(require 'cl)

(defconst user-sj-p (string-match "^\\(sj\\|sudish\\|joseph\\)$"
				(user-login-name))
  "Non-nil iff the user is me.")

;; Initialize load-path and exec-path really early on
(load "~/gnuemacs/init/path-init")

;; custom
(setq custom-file "~/.custom")
(load "~/.custom")

(require 'viper-util)
(require 'loadhist)

;; viper
(setq viper-mode t)
(require 'viper)

;; System-specific init
(if (and window-system (eq system-type 'darwin))
    (load "darwin-init"))

;; Quo vadis?
(setq user-mail-address "sudish@gmail.com")
(defvar user-name-string
  (concat (capitalize (user-full-name)) " <" user-mail-address ">")
  "User's fullname & email address \"fullname <email address>\"")

;; Info search path
(setq Info-additional-directory-list
      (mapcar 'expand-file-name
	      '("/opt/local/share/info"
		"/usr/share/info"
		"/usr/info"
		"~/gnuemacs/site-lisp/cedet/ede"
		"~/gnuemacs/site-lisp/cedet/eieio"
		"~/gnuemacs/site-lisp/cedet/semantic/doc"
		"~/gnuemacs/site-lisp/cedet/speedbar")))

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

(setq track-eol 		t
      scroll-step 		0
      scroll-conservatively     100
      next-screen-context-lines 2
      column-number-mode	t
      require-final-newline 	'ask
      signal-error-on-buffer-boundary nil
      kill-whole-line 		t
      mouse-yank-at-point	t
      comint-scroll-to-bottom-on-output t
      backup-by-copying 		nil
      backup-by-copying-when-linked 	t
      backup-by-copying-when-mismatch 	t
      cursor-in-non-selected-windows    nil
      enable-recursive-minibuffers 	t
      enable-local-eval                 'ask)

(setq default-truncate-lines nil)
(setq-default truncate-lines         nil
	      next-line-add-newlines nil)

;; garbage collection settings
(setq gc-cons-threshold (* 4 1024 1024))

;; better autosave, in a fixed directory
(setq auto-save-default 	t
      auto-save-interval 	300
      auto-save-time-interval 	900
      auto-save-list-file-name	nil
      auto-save-list-file-prefix (expand-file-name "~/autosave/")
      auto-save-directory 	(expand-file-name "~/autosave/"))
;(require 'auto-save)

(require 'font-lock)
(make-face-bold 'modeline)

;; various packages
(load "face-init")
(load "lang-init")
(load "mode-init")
(load "keymap-init")
(load "mail-init")

;; gnuserv
(setenv "GNUSERV_SHOW_EMACS" "1")        ;; always raise Emacs window
;(when (memq window-system '(x ns win32 w32))
;  (require 'gnuserv)
;  (gnuserv-start))

;; file-cache: caches names of files for use from minibuffer
;; initialize the cache as the very last thing we do, once load-path is 
;; fully initialized
(file-cache-add-directory-list load-path)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
