;;; -*- emacs-lisp -*-
;;; $Id: .vip,v 1.9 2001/05/25 05:02:02 sj Exp $

(setq viper-expert-level 5
      viper-want-ctl-h-help t
      viper-no-multiple-ESC 'twice
      viper-ex-style-motion nil
      viper-case-fold-search t
      viper-vi-style-in-minibuffer t
      viper-inhibit-startup-message t)
(setq-default viper-auto-indent t
	      viper-electric-mode t
	      viper-syntax-preference 'extended)

(defconst sj/viper-retained-global-bindings ["\C-d" "\C-u" "\C-w"
					       "\C-t" "\C-e" "\C-y" "\C-v"])
(mapc 
 #'(lambda (key)
;    (define-key viper-emacs-global-user-map  key (lookup-key global-map key))
     (define-key viper-vi-global-user-map     key (lookup-key global-map key))
     (define-key viper-insert-global-user-map key (lookup-key global-map key)))
 sj/viper-retained-global-bindings)
