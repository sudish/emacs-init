;;; mode-init --- various modes we use   [sj--95/11/06]
;;;

;; show-paren
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; paredit mode
(require 'paredit)
(define-key paredit-mode-map (kbd ")") 'paredit-close-brocket-and-newline)
(define-key paredit-mode-map (kbd "M-)") 'paredit-close-brocket)
(mapc #'(lambda (hook)
	  (add-hook hook #'(lambda () (paredit-mode 1))))
      '(emacs-lisp-mode-hook lisp-mode-hook slime-repl-mode-hook))

;; cua-mode
(setq cua-enable-cua-keys nil
      cua-highlight-region-shift-only t
      cua-toggle-set-mark nil)
(cua-mode)

;; project-root
(require 'project-root)
(setq project-roots
      `(("Rails Project"
         :root-contains-files ("app" "config" "db" "lib" "script" "test")
         :on-hit (lambda (p) (message (car p))))
	("Emacs config"
         :path-matches ,(format "\\(%s\\)" sj/emacs-base-dir)
	 :on-hit (lambda (p) (message (car p))))
	("Perl Project"
         :root-contains-files ("t" "lib")
         :on-hit (lambda (p) (message (car p))))))
(global-set-key (kbd "C-c p f") 'project-root-find-file)
(global-set-key (kbd "C-c p g") 'project-root-grep)
(global-set-key (kbd "C-c p a") 'project-root-ack)
(global-set-key (kbd "C-c p d") 'project-root-goto-root)
(global-set-key (kbd "C-c p M-x")
		'project-root-execute-extended-command)
(global-set-key
 (kbd "C-c p v")
 #'(lambda ()
     (interactive)
     (with-project-root
	 (let ((root (cdr project-details)))
	   (cond
	    ((file-exists-p ".svn")
	     (svn-status root))
	    ((file-exists-p ".git")
	     (git-status root))
	    (t
	     (vc-directory root nil)))))))

;; icomplete: incremental minibuffer completion
(require 'icomplete)
(icomplete-mode)

;; complete: partial completion etc.  must be before ffap
(require 'complete)
(partial-completion-mode t)

;; ffap: find file at point (now using ido)
(require 'ffap)
;; (ffap-bindings)
;; (require 'ffap-url)
;; (setq ffap-url-fetcher 'ffap-url-fetcher)

;; new-dabbrev: dabbrev across selectable buffers, dabbrev completion, etc.
(setq dabbrev-always-check-other-buffers t
      dabbrev-abbrev-char-regexp 	"\\sw\\|\\s_"
      dabbrev-case-fold-search 		'case-fold-search
      dabbrev-case-replace 	        'case-replace)

;; diff
(setq diff-switches '("-u"))

;; vc: the other way to access cvs
;(setq vc-diff-switches '("-u" "-kk" "-N"))
(eval-when-compile
  (require 'vc))
(add-to-list 'vc-handled-backends 'Git)

;; magit -- git interface
(sj/load-path-prepend (sj/emacs-path 'ext "magit"))
(autoload 'magit-status "magit" nil t)

;; yasnippet -- dynamic template expansion
(sj/load-path-prepend (sj/emacs-path 'ext "yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (sj/emacs-path 'ext "yasnippet/snippets"))

;; color-theme
;; (sj/load-path-prepend "color-theme")
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-jonadabian-slate)
;; (color-theme-robin-hood)

;; sgml
(setq sgml-quick-keys t)

;; igrep -- a better interface to the greps
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload 'igrep-find "igrep" "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)
(autoload 'grep "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload 'egrep "igrep" "*Run `egrep`..." t)
(autoload 'fgrep "igrep" "*Run `fgrep`..." t)
(autoload 'agrep "igrep" "*Run `agrep`..." t)
(autoload 'grep-find "igrep" "*Run `grep` via `find`..." t)
(autoload 'egrep-find "igrep" "*Run `egrep` via `find`..." t)
(autoload 'fgrep-find "igrep" "*Run `fgrep` via `find`..." t)
(autoload 'agrep-find "igrep" "*Run `agrep` via `find`..." t)

;; filladapt
(require 'filladapt)
(setq-default filladapt-mode t)
(when nil
  (setq-default filladapt-mode t)
  (autoload 'turn-off-filladapt-mode "filladapt" nil t)
  (add-hook 'emacs-lisp-mode-hook 'turn-off-filladapt-mode)
  (add-hook 'change-log-mode-hook 'turn-off-filladapt-mode))

;; greedy-delete
(require 'greedy-delete)

;; ispell
(setq ispell-program-name "/opt/local/bin/ispell")

;; changelog mode
(setq add-log-full-name (if user-sj-p "Sudish Joseph" (user-full-name))
      add-log-mailing-address user-mail-address)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; ido -- a better iswitchb
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point t
      ido-max-work-file-list 30)

;; recentf -- recently visited files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(defun sj/ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'sj/ido-choose-from-recentf)

;; iswitchb
;; (setq iswitchb-default-method 'always-frame)
;; (setq-default iswitchb-method 'always-frame)
;; (iswitchb-mode 1)
;; (when nil
;;   (setq iswitchb-buffer-ignore '("Minibuf")
;; 	iswitchb-regexp nil)
;;   (defvar sj/iswitchb-at-end-regexps '("^ ") ;'("^\\*" "^\\." "^ ")
;;     "Buffers matching these regexps are moved to the end of the buffer list.

;; Buffers are moved in the order of the regexps in the list.  I.e., buffers
;; matching later regexps will be moved further back in the list.")
;;   (defun sj/iswitchb-move-to-end-1 (regexp)
;;  ;; iswitchb-buffer-ignore is too drastic.  moving stuff to the end of
;;     ;; the offered list is preferable for some stuff.
;;     ;; `iswitchb-temp-buflist' is magical, ugh.
;;     (let ((hidden nil))
;;       (dolist (buf iswitchb-temp-buflist)
;; 	(if (string-match regexp buf)
;; 	    (push buf hidden)))
;;       (iswitchb-to-end hidden)))
;;   (defun sj/iswitchb-move-to-end ()
;;     (mapc 'sj/iswitchb-move-to-end-1 sj/iswitchb-at-end-regexps))
;;   (add-hook 'iswitchb-make-buflist-hook 'sj/iswitchb-move-to-end))

(setq dired-compression-method 'gzip
      dired-dwim-target t
      dired-find-subdir t
      dired-keep-marker-copy t
      dired-mail-reader 'vm
      dired-copy-preserve-time t
      dired-backup-if-overwrite 'always
      dired-grep-zcat-program "gzcat")
(setq dired-auto-shell-command-alist ;; see dired-shell.el
      '(("\\.tar\\.\\([zZ]\\|gz\\)\\|\\.tgz$"
	 (if (memq system-type '(hpux))
	     "gzcat * | gtar xvvf -" ;; gtar's broken (install.?) on the hp's
	   "gtar xzvvf *")
	 (if (memq system-type '(hpux))
	     "gzcat * | gtar tvvf -"
	   "gtar tzvvf *"))))
(setq dired-no-confirm
      '(byte-compile chgrp chmod chown compress hardlink load
        move print shell symlink uncompress patch create-top-dir
        revert-subdirs))
(defun sj/dired-after-readin-hook ()
  (setq truncate-lines t))
(add-hook 'dired-after-readin-hook 'sj/dired-after-readin-hook)

;; ibuff-menu: better buffer menu
;; (global-set-key "\C-x\C-b" 'ibuff-menu)
;; (autoload 'ibuff-menu "ibuff-menu" "Edit the buffer list." t)
;; (setq ibuff-show-buffer-size-in-menu t)
;; (setq ibuff-hide-buffers-regexp
;;       "^\\( .*\\|\\*[^s].*\\|INBOX.*\\|\\.diary\\|\\.bbdb\\|\\.bibl\\|sent .*\\|\\*ftp .*\\)$")

;; ibuffer -- ya improved buffer menu, included with emacs 22+
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("Ruby" (mode . ruby-mode))
	 ("Shell" (mode . sh-mode))
	 ("Emacs" (or
		   (mode . emacs-lisp-mode)
		   (name . "^\\*scratch\\*$")
		   (name . "^\\*Messages\\*$")))
	 ("Gnus" (or
		  (mode . message-mode)
		  (mode . bbdb-mode)
		  (mode . mail-mode)
		  (mode . gnus-group-mode)
		  (mode . gnus-summary-mode)
		  (mode . gnus-article-mode)
		  (name . "^\\.bbdb$")
		  (name . "^\\.newsrc-dribble")))
	 ("Dired" (mode . dired-mode))
	 ("Perl" (mode . cperl-mode)))))
(add-hook 'ibuffer-mode-hook
	  #'(lambda ()
	      (ibuffer-switch-to-saved-filter-groups "default")))

;; allout: better outline mode
(setq outline-mode-leaders '((c-mode . "/\\*\\*\\*_")
			     (perl-mode . "###_")))
;(load "allout" nil t)
;(outline-init t)

;; gnus5
(defun sj/gnus (&optional level)
  "Wrapper to startup gnus.  Uses level 3 by default."
  (interactive "P")
  ;(require 'gnus-load)
  (gnus (or level 3)))
(defun sj/gnus-just-mail (&optional level)
  "Start gnus at level 2.  Ie., just mail groups."
  (interactive "P")
  (gnus (or level 2)))
(global-set-key "\C-cn" 'sj/gnus)
(global-set-key "\C-cN" 'sj/gnus-just-mail)

;; w3 settings, from Per Abrahamsen
(setq w3-user-fonts-take-precedence t
      w3-user-colors-take-precedence t
      w3-honor-stylesheets nil
      w3-use-terminal-characters nil
      w3-horizontal-rule-char 45)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
