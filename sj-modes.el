;;; sj-modes --- Initialize various modes and packages
;;
;; Copyright: Sudish Joseph <sudish@gmail.com>
;; Created: 1995-06-11

(defconst sj/lisp-mode-hooks '(emacs-lisp-mode-hook
			       lisp-mode-hook
			       clojure-mode-hook
			       slime-repl-mode-hook)
  "Hooks for modes handling lisp-like languages.")

;; show-paren
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; parenface: dim the color for parens
(require 'parenface)

;; highlight-parentheses: highlights currently enclosing sexps
(require 'highlight-parentheses)
(mapc (lambda (hook)
	(add-hook hook 'highlight-parentheses-mode))
      sj/lisp-mode-hooks)

;; paredit mode
(autoload 'paredit-mode "paredit" "paredit mode" t)
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "RET") 'paredit-newline)
     (define-key paredit-mode-map (kbd "C-j") nil)))
(defun sj/paredit-mode-hook ()
  (paredit-mode 1)
  (set (make-local-variable 'viper-insert-local-user-map) (make-keymap))
  (mapc (lambda (keys)
	  (define-key viper-insert-local-user-map (car keys)
	    (lookup-key paredit-mode-map (cdr keys))))
	`(("\d"			. "\d")
	  (,(kbd "<backspace>") . "\d")
	  ("\C-d"		. "\C-d")
	  (,(kbd "<delete>")	. "\C-d"))))
(mapc (lambda (hook)
	(add-hook hook 'sj/paredit-mode-hook))
      sj/lisp-mode-hooks)

;; cua-mode
(setq cua-enable-cua-keys nil
      cua-highlight-region-shift-only t
      cua-toggle-set-mark nil)
(cua-mode)

;; ibuffer -- ya improved buffer menu, included with emacs 22+
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("C/C++" (or
		   (mode . c-mode)
		   (mode . c++-mode)
		   (mode . makefile-mode)
		   (mode . makefile-bsdmake-mode)))
	 ("Ruby"   (mode . ruby-mode))
	 ("Python" (mode . python-mode))
	 ("Erlang" (mode . erlang-mode))
	 ("Shell"  (mode . sh-mode))
	 ("Perl"   (mode . cperl-mode))
	 ("Lisp"  (or
		   (mode . lisp-mode)
		   (name . "^\\*slime-repl")))
	 ("Emacs" (or
		   (mode . emacs-lisp-mode)
		   (name . "^\\*scratch\\*$")))
	 ("IRC"    (mode . erc-mode))
	 ("Gnus"  (or
		   (mode . message-mode)
		   (mode . bbdb-mode)
		   (mode . mail-mode)
		   (mode . gnus-group-mode)
		   (mode . gnus-summary-mode)
		   (mode . gnus-article-mode)
		   (name . "^\\.bbdb$")
		   (name . "^\\.newsrc-dribble")))
	 ("Dired"  (mode . dired-mode))
	 ("Git"   (or
		   (mode . magit-mode)
		   (name . "magit")))
	 ("Slime misc" (or
			(name . "^\\*inferior-lisp")
			(name . "^\\*slime"))))))
(add-hook 'ibuffer-mode-hook
	  #'(lambda ()
	      (ibuffer-switch-to-saved-filter-groups "default")))

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

;; icomplete: incremental minibuffer completion
(require 'icomplete)
(icomplete-mode)

;; complete: partial completion etc.  must be before ffap
(require 'complete)
(partial-completion-mode t)

;; ffap: find file at point (now using ido)
;; (require 'ffap)
;; (ffap-bindings)
;; (require 'ffap-url)
;; (setq ffap-url-fetcher 'ffap-url-fetcher)

;; pabbrev
;; (require 'pabbrev)
;; (global-pabbrev-mode)

;; diff
(setq diff-switches '("-u"))

;; vc: the other way to access cvs
;(setq vc-diff-switches '("-u" "-kk" "-N"))
(eval-when-compile
  (require 'vc))
(add-to-list 'vc-handled-backends 'Git)

;; magit -- git interface
(sj/load-path-prepend "external/magit" t)
(autoload 'magit-status "magit" nil t)

;; yasnippet -- dynamic template expansion
;; (sj/load-path-prepend "external/yasnippet")
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory (sj/emacs-path "external/yasnippet/snippets"))

;; sgml
(setq sgml-quick-keys t)

;; igrep -- a better interface to the greps
;; (autoload 'igrep "igrep"
;;   "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
;; (autoload 'igrep-find "igrep" "*Run `grep` via `find`..." t)
;; (autoload 'igrep-visited-files "igrep"
;;   "*Run `grep` ... on all visited files." t)
;; (autoload 'dired-do-igrep "igrep"
;;   "*Run `grep` on the marked (or next prefix ARG) files." t)
;; (autoload 'dired-do-igrep-find "igrep"
;;   "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
;; (autoload 'Buffer-menu-igrep "igrep"
;;   "*Run `grep` on the files visited in buffers marked with '>'." t)
;; (autoload 'igrep-insinuate "igrep"
;;   "Define `grep' aliases for the corresponding `igrep' commands." t)
;; (autoload 'grep "igrep"
;;   "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
;; (autoload 'egrep "igrep" "*Run `egrep`..." t)
;; (autoload 'fgrep "igrep" "*Run `fgrep`..." t)
;; (autoload 'agrep "igrep" "*Run `agrep`..." t)
;; (autoload 'grep-find "igrep" "*Run `grep` via `find`..." t)
;; (autoload 'egrep-find "igrep" "*Run `egrep` via `find`..." t)
;; (autoload 'fgrep-find "igrep" "*Run `fgrep` via `find`..." t)
;; (autoload 'agrep-find "igrep" "*Run `agrep` via `find`..." t)

;; filladapt
(require 'filladapt)
(setq-default filladapt-mode t)

;; greedy-delete
(require 'greedy-delete)
(mapc (lambda (hook)
	(add-hook hook 'gd-add-to-mode))
      '(c-mode-hook c++-mode-hook ruby-mode-hook erlang-mode-hook
		    emacs-lisp-mode-hook lisp-mode-hook clojure-mode-hook
		    haskell-mode-hook))

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
      ido-all-frames t
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

;; dired
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
