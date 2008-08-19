;;; init --- main init file   [sj--95/06/11]
;;;

;; I wish cl were the default
(require 'cl)

(defconst user-sj-p (string-match "^\\(sj\\|sudish\\|joseph\\)$"
				(user-login-name))
  "Non-nil iff the user is me.
Used to protect people who copy this from potential havoc.

If you want all the gimmicks, put your login name in the above and
throw away your non-warranty.")

;; Initialize load-path really early on
(load "~/gnuemacs/init/load-path-init")

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
		"/usr/info")))

;; load misc. functions we need later
(load "misc-init")

;; Take off some of the training wheels.
(when user-sj-p
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'eval-expression 'disabled nil))

(setq default-major-mode 'indented-text-mode)
(add-hook 'text-mode-hook
	  '(lambda ()
	     (auto-fill-mode t)
	     (setq adaptive-fill-mode t)))

(setq track-eol 		t
      scroll-step 		0
      scroll-conservatively     100
      next-screen-context-lines 2
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

;; 8 bit character support
;(standard-display-european t)
;(set-input-mode (car (current-input-mode)) (cadr (current-input-mode)) t)

;; garbage collection settings
(setq gc-cons-threshold 1048576)

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

;; gnus5
(defun sj/gnus (&optional level)
  "Wrapper to startup gnus.  Uses level 3 by default."
  (interactive "P")
  ;(require 'gnus-load)
  (gnus (or level 3)))
(defun sj/gnus-just-mail (&optional level)
  "Start gnus at level 2.  Ie., just mail groups."
  (interactive "P")
  ;(require 'gnus-load)
  ;; workaround for old XEmacs bug
  ;;(push (character-to-event ?l) unread-command-events)
  (gnus (or level 2)))
(global-set-key "\C-cn" 'sj/gnus)
(global-set-key "\C-cN" 'sj/gnus-just-mail)

;; icomplete: incremental minibuffer completion
(require 'icomplete)
(setq icomplete-dynamic-default nil)

;; complete: partial completion etc.  must be before ffap
(load "complete")
(partial-completion-mode t)

;; ffap: find file at point
(setq ffap-require-prefix t)
(autoload 'gnus-ffap-next-url "ffap") ; if ffap is not preloaded
(require 'ffap)
(ffap-bindings)
(require 'ffap-url)
(setq ffap-url-fetcher 'ffap-url-fetcher)

;; ff-paths: much-enhanced find-file
(setq ff-paths-list
  '(("\\.awk$" "$AWKPATH")              ; awk files in AWKPATH env variable.
    ("^\\." "~/")                       ; .* (dot) files in user's home
    ("\\.el$" load-path)))
(setq ff-paths-display-non-existant-filename nil
      ff-paths-use-locate nil)
(require 'ff-paths)

;; new-dabbrev: dabbrev across selectable buffers, dabbrev completion, etc.
(setq dabbrev-always-check-other-buffers t
      dabbrev-abbrev-char-regexp 	"\\sw\\|\\s_"
      dabbrev-case-fold-search 		'case-fold-search
      dabbrev-case-replace 	        'case-replace)

;; namedmarks: multiple, named, per-buffer marks
;(require 'namedmarks)

;; for metamail
(setenv "X_VIEWER" "xv -geometry +1+1")
(setenv "METAMAIL_TMPDIR" "/tmp/")

;; diff
(setq diff-switches '("-u"))

;; gnuserv
(setenv "GNUSERV_SHOW_EMACS" "1")        ;; always raise Emacs window
;(when (memq window-system '(x mac win32 w32))
;  (require 'gnuserv)
;  (gnuserv-start))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
