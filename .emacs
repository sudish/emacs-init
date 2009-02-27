;; EMACS startup file

;; Always start in $HOME
(cd (expand-file-name "~"))

(defvar sj/emacs-type
  (cond ((string-match "xemacs\\|lucid" emacs-version) 'xemacs)
	(t 'emacs)))

;; Base directory for all my emacs files.
(setq sj/emacs-base-dir	(expand-file-name "~/gnuemacs"))
;; (setq sj/emacs-base-dir
;;       (expand-file-name
;;        (cond
;; 	((eq sj/emacs-type 'xemacs) "~/xemacs")
;; 	((eq sj/emacs-type 'emacs)  "~/gnuemacs")
;; 	(t (error "Unknown Emacs type '%s' in ~/.emacs" sj/emacs-type)))))

;; Load the path-handling code
(load (concat sj/emacs-base-dir "/init/sj-paths"))

;; Make sure the init and site-lisp dirs are on load-path
(sj/load-path-prepend '("init" "site-lisp"))

;; Load sj-init.el, which then bootstraps everything else
(load "sj-init")

;; The message below appears only on succesful loading of all the init
;; files, making it easier to notice failed loading
(defun sj/after-init-hook ()
  (message (emacs-version)))
(add-hook 'after-init-hook 'sj/after-init-hook 'append)
(setq inhibit-startup-echo-area-message "sj")
