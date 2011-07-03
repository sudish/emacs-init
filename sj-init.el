;;; sj-init --- Main initialization file, called from ~/.emacs
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

;; Track every loaded feature and the symbols they bring in
(require 'loadhist)

;; Common Lisp emulation
(require 'cl)

(defconst user-sj-p (string-match "^\\(sj\\|sudish\\|joseph\\)$"
				(user-login-name))
  "Non-nil iff the user is me.")

;; Custom
(setq custom-file "~/.custom")
;(load custom-file)

;; Viper mode
(setq viper-mode t)
(require 'viper)

;; System-specific init
(cond ((eq system-type 'darwin)
       (load "sj-system-darwin"))
      ((eq system-type 'gnu/linux)
       (load "sj-system-linux")))

;; Load misc. functions we need later
(load "sj-funcs")

;; Enable usage of ssh-agent from sub-shells
(sj/copy-login-env-vars '("SSH_AUTH_SOCK" "SSH_AGENT_PID")
			"source $HOME/.keychain/sudish-sh")
(let ((ssh-agent-socket (getenv "SSH_AUTH_SOCK")))
  (unless (and (file-writable-p ssh-agent-socket)
	       (not (file-regular-p ssh-agent-socket)))
    (error "Can't locate $SSH_AUTH_SOCK (%s)" ssh-agent-socket)))

;; Quo vadis?
(setq user-full-name    "Sudish Joseph"
      user-mail-address "sudish@gmail.com"
      user-name-string  (concat user-full-name " <" user-mail-address ">"))

;; Info search path
(setq Info-additional-directory-list
      (delq nil
	    (mapcar (lambda (dir)
		      (cond ((file-directory-p dir) dir)
			    (t nil)))
		    '("/opt/local/share/info"
		      "/sw/share/info"
		      "/Developer/usr/share/info"
		      "/usr/share/info"
		      "/usr/info"))))

;; Take off some of the training wheels.
(when user-sj-p
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'eval-expression 'disabled nil))

(setq-default major-mode 'indented-text-mode)
(add-hook 'text-mode-hook
	  (defun sj/text-mode-hook ()
	    (auto-fill-mode t)
	    (setq adaptive-fill-mode t)))

;; Show the region
(transient-mark-mode t)

;; No tool bar, please
(tool-bar-mode -1)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Better display of long lines
(setq visual-line-fringe-indicators	; always display wrap indicators
      (cdr (assq 'continuation fringe-indicator-alist)))
(global-visual-line-mode)

;; Tweak some of the defaults
(setq track-eol 		t
      scroll-step 		0
      scroll-conservatively     most-positive-fixnum
      scroll-preserve-screen-position 'always
      line-move-visual          nil
      next-screen-context-lines 2
      confirm-kill-emacs        'y-or-n-p
      inhibit-startup-screen    t
      initial-buffer-choice     t
      visible-bell		t
      column-number-mode	t
      require-final-newline 	'ask
      signal-error-on-buffer-boundary nil
      kill-whole-line 		t
      mouse-yank-at-point	t
      blink-cursor-delay        1.0
      backward-delete-char-untabify-method 'hungry
      pp-escape-newlines        nil
      enable-recursive-minibuffers 	t
      enable-local-eval                 'ask
      safe-local-variable-values        '((sj/recompile-file . t)))

;; Don't want warnings about using funcs from cl.el
(setq byte-compile-warnings '(not cl-functions))

;; help system
(setq  help-window-select        'always
       help-at-pt-display-when-idle '(keymap local-map button kbd-help
					     help-echo))
(help-at-pt-set-timer)

(setq query-replace-interactive nil
      query-replace-show-replacement t
      lazy-highlight-initial-delay 0)

(setq-default truncate-lines nil)
(setq next-line-add-newlines nil)

(setq-default indicate-empty-lines nil
	      indicate-buffer-boundaries '((top . left) (bottom . left)))

;; backups
(setq backup-by-copying 		nil
      backup-by-copying-when-linked 	t
      backup-by-copying-when-mismatch 	t
      ;; fixed directory for backups
      backup-directory-alist 		`(("." . ,(concat user-emacs-directory
							  "backups/")))
      ;; versioned backups
      delete-old-versions 		t
      kept-new-versions 		2
      kept-old-versions 		0
      version-control 			nil)

;; auto-saves
(defconst sj/auto-save-directory (concat user-emacs-directory "auto-saves/"))
(make-directory sj/auto-save-directory t)
(setq auto-save-default  t
      auto-save-interval 300
      ;; fixed directory for auto-saves
      auto-save-list-file-prefix (concat sj/auto-save-directory ".saves-")
      auto-save-file-name-transforms `(,@auto-save-file-name-transforms
				       (".*" ,sj/auto-save-directory t)))

;; garbage collection settings
(setq gc-cons-threshold (max gc-cons-threshold (* 4 1024 1024)))

(make-face-bold 'modeline)

;; various packages
(load "sj-keymaps")
(load "sj-modes")
(load "sj-langs")
(load "sj-anything")
(load "sj-mail")
(load "sj-bbdb")

;; gnuserv
(setenv "GNUSERV_SHOW_EMACS" "1")        ; always raise Emacs window
(when (memq system-type '(darwin gnu/linux))
  (server-start))

;; emacs_chrome edit-server: service requests from the Chrome extension
;; (sj/load-path-prepend "external/emacs_chrome/servers")
;; (require 'edit-server)
;; (setq edit-server-verbose t
;;       edit-server-new-frame-alist nil)
;; (condition-case err
;;     (edit-server-start)
;;   (error
;;    (message "edit-server: %s" (error-message-string err))))

;; smex: IDO for interactive commands (an IDO-enabled M-x)
;; (sj/load-path-prepend "external/smex")
;; (require 'smex)
;; (setq smex-save-file (concat user-emacs-directory ".smex")
;;       smex-history-length 15)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;; (global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; filecache: caches names of files for use from minibuffer.
;; initialize the cache as the very last thing we do, once load-path is
;; fully initialized
(file-cache-add-directory-list load-path)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
