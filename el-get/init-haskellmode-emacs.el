;;; init-haskellmode-emacs ---    [sj--12/02/18]

;; Haskell mode
;; (eval-when-compile
;;   (require 'scion)  
;;   (require 'haskell-mode)
;;   (require 'haskell-indentation))
(setq haskell-program-name "ghci"
      inferior-haskell-wait-and-jump t
      haskell-indent-offset 4
      haskell-indent-look-past-empty-line nil
      haskell-font-lock-symbols nil)
(setq-default haskell-doc-show-global-types t)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(defun sj/haskell-mode-hook ()
  ;; (setq comment-start   "--"
  ;; 	comment-padding " ")
  (set (make-local-variable 'require-final-newline) t)
  (turn-on-haskell-decl-scan)
  (turn-on-haskell-doc-mode)
  ;; haskell-indent.el is too complex by far
  ;(turn-on-haskell-indent)
  ;; haskell-indentation.el seems nice & simple
  (haskell-indentation-mode)
  ;; from http://github.com/tibbe/haskell-style-guide
  ;; (setq tab-width 4
  ;; 	haskell-indentation-layout-offset 4
  ;; 	haskell-indentation-left-offset 4
  ;; 	haskell-indentation-ifte-offset 4)
  (auto-fill-mode)
  ;; Take haskell-indentation keys back from Viper's intrusive grip
  (setq viper-insert-local-user-map
	(sj/copy-keys-from-keymap haskell-indentation-mode-map
				  '([?\C-d] [backspace] [?\r]
				    ([backspace] . [delete])
				    ([backspace] . [?\d]))))
  (scion-mode 1)
  (scion-flycheck-on-save 1)
  (ghc-init)
  (flymake-mode))
(add-hook 'haskell-mode-hook 'sj/haskell-mode-hook)
(add-to-list 'viper-emacs-state-mode-list 'inferior-haskell-mode)
;; (eval-after-load 'filladapt
;;   '(progn
     (add-to-list 'filladapt-token-table '("-- " haskell-comment))
     (add-to-list 'filladapt-token-match-table '(haskell-comment haskell-comment))
     (add-to-list 'filladapt-token-conversion-table '(haskell-comment . exact))
     ;; ))
;; Select the inferior-haskell window when it's displayed.
(defun sj/select-inf-haskell-buffer ()
  "Display and select the buffer used by the inferior Haskell process, if any."
  (interactive)
  (when (processp (inferior-haskell-process))
    (let ((buf (process-buffer (inferior-haskell-process))))
      (select-window (display-buffer buf)))))
;; required for defadvice preactivation below
(eval-when-compile (require 'inf-haskell))
(defadvice inferior-haskell-load-file (after sj/select-inferior-haskell
					     preactivate compile)
  "Display and select the inferior haskell buffer."
  (sj/select-inf-haskell-buffer))


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
