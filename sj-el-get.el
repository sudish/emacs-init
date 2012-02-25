;;; sj-el-get --- el-get init   [sj--12/02/08]

(setq el-get-byte-compile-at-init nil
      el-get-user-package-directory "~/.emacs.d/lib/init/el-get")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; local sources
(setq el-get-sources
      '((:name anything
	       :features (anything anything-match-plugin anything-config anything-show-completion))
	(:name auto-complete
	       :after (lambda ()
			(setq ac-use-comphist t
			      ac-disable-faces nil ; '(font-lock-comment-face font-lock-string-face font-lock-doc-face)
			      ac-auto-show-menu 0.1
			      ac-quick-help-delay 0.1
			      ac-menu-height 20
			      ac-quick-help-height 20)
			(global-auto-complete-mode t)
			(require 'auto-complete-config)
			(ac-config-default)))
	auto-complete-clang
	auto-complete-css
	auto-complete-emacs-lisp
	auto-complete-extension
	auto-complete-ruby
	autopair
	bbdb
	coffee-mode
	color-theme
	(:name color-theme-sanityinc-solarized
	       :features color-theme-sanityinc-solarized
	       :after (lambda ()
			(set-default 'frame-background-mode 'light)
			(require 'color-theme-sanityinc-solarized)
			(color-theme-sanityinc-solarized-light)))
	(:name distel
	       :features nil
	       :after (lambda ()
			(eval-after-load 'erlang
			  '(distel-setup))))
	el-get
	(:name emacschrome
	       :features edit-server
	       :after (lambda ()
			;; emacs_chrome edit-server: service requests from the Chrome extension
			(setq edit-server-verbose t
			      edit-server-new-frame nil
			      edit-server-new-frame-alist nil)
			(condition-case err
			    (edit-server-start)
			  ((error "" &optional ARGS)
			   (message "edit-server: %s" (error-message-string err))))))
	(:name eproject
	       :features (eproject eproject-extras eproject-ruby eproject-ruby-on-rails eproject-perl))
	erlware-mode
	(:name filladapt
	       :features filladapt
	       :after (lambda () (setq-default filladapt-mode t)))
	ghc-mod
	(:name haskellmode-emacs
	       :depends filladapt) ; init file
	highlight-parentheses
	highlight-symbol
	ibuffer-vc
	iedit
	inf-ruby
	(:name magit
	       :after (lambda ()
			(add-to-list 'viper-emacs-state-mode-list 'magit-key-mode)))
	markdown-mode
	minimap
	(:name paredit
	       :after (lambda ()
			(define-key paredit-mode-map (kbd "RET") 'paredit-newline)
			(define-key paredit-mode-map (kbd "C-j") nil)))
	pos-tip
	(:name rainbow-delimiters
	       :features rainbow-delimiters
	       :after (lambda () (global-rainbow-delimiters-mode)))
	rhtml-mode
	(:name ruby-mode
	       :after (lambda ()
			(setq ruby-deep-indent-paren nil
			      ruby-deep-arglist nil)
			(add-to-list 'viper-vi-state-mode-list 'ruby-mode)
			(eval-after-load 'ruby-mode
			  '(add-hook 'ruby-mode-hook
				     (defun sj/ruby-mode-hook ()
				       (highlight-parentheses-mode t)
				       (setq autopair-dont-activate t ; ruby-electric
					     show-trailing-whitespace nil))))))
	ruby-electric
	scion
	(:name simplenote
	       :after (lambda ()
			(setq simplenote-email user-mail-address)))
	(:name smex
	       :after (lambda ()
			(setq smex-save-file (concat user-emacs-directory ".smex")
			      smex-history-length 15)
			(smex-initialize)
			(global-set-key (kbd "M-x") 'smex)
			(global-set-key (kbd "M-X") 'smex-major-mode-commands)
			;; This is the old M-x.
			(global-set-key (kbd "C-c M-x") 'execute-extended-command)))
	(:name undo-tree
	       :after (lambda () (global-undo-tree-mode)))
	(:name volatile-highlights
	       :features volatile-highlights
	       :after (lambda ()
			(require 'volatile-highlights)
			(volatile-highlights-mode t)))
	vc-bzr
	vc-darcs
	(:name yaml-mode
	       :after (lambda ()
			(eval-after-load 'yaml-mode
			  '(define-key yaml-mode-map "\C-m" 'newline-and-indent))))
	;; (:name yasnippet
	;;        :features yasnippet
	;;        :after (lambda () (yas/global-mode 1)))
	))

(el-get)

;;; Archived

	;; (:name clojure-mode
	;;        :after (lambda ()
	;; 		(setq clojure-mode-use-backtracking-indent t
	;; 		      clojure-mode-font-lock-comment-sexp t)
	;; 		(autoload 'clojure-mode "clojure-mode" "A mode for clojure lisp" t)
	;; 		(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
	;; 		(add-to-list 'viper-vi-state-mode-list 'clojure-mode)
	;; 		(autoload 'clojure-indent-function "clojure-mode") ; for use in slime buffers
	;; 		(eval-when-compile (require 'parenface))
	;; 		(eval-after-load 'clojure-mode
	;; 		  '(add-hook 'clojure-mode-hook
	;; 			     (paren-face-add-support clojure-font-lock-keywords)))))
	;; (:name slime
	;;        :features slime
	;;        :after (lambda ()
	;; 		(setq slime-default-lisp 'clojure
	;; 		      slime-inhibit-pipelining nil
	;; 				;slime-protocol-version 'ignore ; don't warn on mismatch
	;; 		      slime-use-autodoc-mode nil ; swank-clojure bug
	;; 		      slime-autodoc-use-multiline-p t
	;; 		      slime-net-coding-system 'utf-8-unix)
	;; 		(add-hook 'slime-connected-hook 'slime-redirect-inferior-output)
	;; 		;; select the contrib/extra packages we want
	;; 		(slime-setup
	;; 		 '(slime-scratch slime-repl slime-fancy
	;; 				 slime-c-p-c ;slime-fuzzy
	;; 				 slime-fontifying-fu slime-editing-commands
	;; 				 slime-sbcl-exts))
	;; 		(define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
	;; 		(define-key slime-mode-map (kbd "C-j") 'newline)
	;; 		(defvar sj/slime-sbcl-path "/usr/local/bin/sbcl")
	;; 		(eval-after-load 'slime
	;; 		  '(add-to-list 'slime-lisp-implementations
	;; 				`(sbcl (,sj/slime-sbcl-path) :coding-system utf-8-unix) t))))
	;; (:name swank-clojure
	;;        :after (lambda ()
	;; 		(setq swank-clojure-compile-p t)
	;; 		;; taken from http://groups.google.com/group/swank-clojure/msg/73fe5c599d854ea5
	;; 		(add-hook 'slime-repl-mode-hook 
	;; 			  (defun sj/slime-clojure-repl-setup () 
	;; 			    (when (string-equal "clojure" (slime-connection-name)) 
	;; 			      (clojure-mode-font-lock-setup) 
	;; 			      (when (slime-inferior-process) 
	;; 				(slime-redirect-inferior-output)) 
	;; 			      (swank-clojure-slime-repl-modify-syntax)))) 
	;; 		;; Use a wrapper shell script to start clojure.  The Clojure classpath
	;; 		;; must contain the swank-clojure jar file for SLIME to run, so we
	;; 		;; compute that here from the Emacs load-path instead of hard-coding
	;; 		;; it into the script.
	;; 		(setq sj/swank-jar-path "external/swank-clojure/swank-clojure.jar"
	;; 		      swank-clojure-binary
	;; 		      `("~/bin/clojure" "-C" ,(sj/emacs-path sj/swank-jar-path)))
	;; 		;; Hook into SLIME manually. swank-clojure now wants you to use maven
	;; 		;; or lein and other overly complicated methods.
	;; 		(eval-after-load 'slime
	;; 		  `(add-to-list 'slime-lisp-implementations
	;; 				'(clojure (,@swank-clojure-binary) :init swank-clojure-init)))))

        ;; (:name rinari
	;;        :after (lambda ()
	;; 		(defun sj/clone-rinari-keymap ()
	;; 		  "Copy Rinari's difficult to reach keymaps to someplace better."
	;; 		  (define-prefix-command 'sj/rinari-main-keymap)
	;; 		  (define-prefix-command 'sj/rinari-jump-keymap)
	;; 		  (set-keymap-parent 'sj/rinari-main-keymap
	;; 				     (lookup-key rinari-minor-mode-map (kbd "C-c ;")))
	;; 		  (set-keymap-parent 'sj/rinari-jump-keymap
	;; 				     (lookup-key rinari-minor-mode-map (kbd "C-c ; f")))
	;; 		  (define-key rinari-minor-mode-map [(super g)] sj/rinari-main-keymap)
	;; 		  (define-key rinari-minor-mode-map [(super j)] sj/rinari-jump-keymap))
	;; 		(eval-after-load 'rinari
	;; 		  '(sj/clone-rinari-keymap))))
        ;; (:name rvm
	;;        (defun sj/rvm-activate ()
	;; 	 "Facilitate per-project .rvmrc files."
	;; 	 (require 'rvm)
	;; 	 (rvm-activate-corresponding-ruby))
	;;        (eval-after-load 'ruby-mode
	;; 	 '(add-hook 'ruby-mode-hook 'sj/rvm-activate)))



;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
