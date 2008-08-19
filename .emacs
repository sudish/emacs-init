;; EMACS startup file

;; You'd think this would be the default in all Emacsen, all the time.
(cd (expand-file-name "~"))

(defvar sj/emacs-type
  (if (string-match "xemacs\\|lucid" emacs-version)
      'xemacs
    'emacs))

;; it's a pity that the two Emacsen have diverged so much that we need
;; separate init file trees
(cond ((eq sj/emacs-type 'xemacs)
       (load (expand-file-name "~/xemacs/init/init")))
      ((eq sj/emacs-type 'emacs)
       (load (expand-file-name "~/gnuemacs/init/init"))))

;; the message below appears only on succesful loading of all the init
;; files, making it easier to notice failed loading
(defun sj/after-init-hook ()
  (message "%sEmacs %s, running on %s (%s)."
	   (if (eq sj/emacs-type 'xemacs) "X" "GNU ")
	   emacs-version (system-name) system-type))
(add-hook 'after-init-hook 'sj/after-init-hook 'append)
(setq inhibit-startup-echo-area-message "sj")
