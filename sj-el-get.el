;;; sj-el-get --- el-get init   [sj--12/02/08]

(setq el-get-byte-compile-at-init nil
      el-get-user-package-directory "~/.emacs.d/lib/init/el-get")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; local sources
(setq el-get-sources
      '((:name clojure-mode
	       :after (progn
			(message "clojure-mode is %s" (featurep 'clojure-mode))))
	el-get
	(:name filladapt
	       :features filladapt
	       :after (progn (setq-default filladapt-mode t)))
	ghc-mod
	(:name haskellmode-emacs
	       :depends filladapt) ; init file
	scion
	vc-bzr
	))

(el-get)

;;; Archived

	;; (:name slime
	;;        :features slime
	;;        :after (progn
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
	;;        :after (progn
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
	;;        :after (progn
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


	;; (:name distel
	;;        :features nil
	;;        :after (progn
	;; 		(eval-after-load 'erlang
	;; 		  '(distel-setup))))

;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
