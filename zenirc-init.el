;; zenirc startup stuff -*- emacs-lisp -*-

(require 'zenirc)

(setq zenirc-server-alist
      '(("event.mindspring.com" nil nil "sj" "Sudish Joseph")))

(setq zenirc-nick-default "sj"
      zenirc-fingerdata "Sudish Joseph")

(setq zenirc-userinfo "Architecturally unsound."
      zenirc-send-confirmation 'message
      zenirc-delete-preceding-whitespaces t
      zenirc-timestamp nil
      zenirc-timestamp-prefix "["
      zenirc-timestamp-suffix "]"
      zenirc-always-show-channelname t
      zenirc-send-ctcp-errmsg-on-unknown t
      zenirc-send-ctcp-errmsg-on-unbalanced t
      zenirc-verbose-ctcp t
      zenirc-verbose-dcc t
      zenirc-whowas-on-401 t)

(setq-default zenirc-fill-type 'static)
(setq zenirc-fill-prefix " | "
      zenirc-fill-message-categories t
      zenirc-fill-outgoing-mode t)

(setq zenirc-notify-list '("dmayne" "keat" "k"))

(setq zenirc-beep-on-signal nil)
;(zenirc-add-hook 'zenirc-server-001-hook
; (defun sj/zenirc-popup (proc parsedmsg)
;   (setq zenirc-signal-list '("bugfarmers"))))

(setq zenirc-color-message-categories t)
(setq zenirc-color-alist
      `(("^\\[info\\]" ,(sj/hilit-lookup-face-create 'red))
	("^\\[trigger\\]" ,(sj/hilit-lookup-face-create 'red-bold))
	("^\\[action" ,(sj/hilit-lookup-face-create 'darkgreen-bold))
	("^\\[time\\]" ,(sj/hilit-lookup-face-create 'brown-bold))
	("\\(esa\\|alexei\\|drew\\)[#@!]" ,(sj/hilit-lookup-face-create 'blue))))

;; this is a list of annoying things to ignore. This list ignores
;; messages from nickserv, anything with the word "fnord" in it,
;; messages from the major dweeb craig and everything with more then
;; four CTCPs in it.
;(setq zenirc-ignore-list
;      '("^:NickServ!Nickserv@hpsystem2.informatik.tu-muenchen.de" "fnord"
;	"^:craig!craig@netsys1.netsys.com"
;	"\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a"))

;; the following is an example of how to do something during initializing a
;; server connection. 001 is the first thing the server sends to a client
;; after the client sends USER and NICK.
;;
;; :pfawww.pp.se 001 Omnion :Welcome to the Internet Relay Network Omnion
;;
(defvar zenirc-startup-channels "#engineers,#portaldev,#wormhole"
  "*Comma separated string of channels to join during startup")
(defun zenirc-startup-join (proc parsedmsg)
  (process-send-string proc
		       (concat "JOIN " zenirc-startup-channels "\n")))
(add-hook 'zenirc-server-001-hook 'zenirc-startup-join)

(autoload 'zenirc "zenirc" nil t)
(add-hook 'zenirc-mode-hook
  (defun sj/zenirc-mode-hook ()
    (require 'zenirc-8ball)
    ;;(require 'zenirc-away)
    ;;(require 'zenirc-random-away)
    (require 'zenirc-color)
    (zenirc-color-mode)
    (require 'zenirc-command-queue)
    (require 'zenirc-complete)
    ;;(require 'zenirc-dcc)
    (require 'zenirc-fill)
    (zenirc-fill-mode)
    (require 'zenirc-format)
    (require 'zenirc-history)
    (require 'zenirc-ignore)
    (require 'zenirc-netsplit)
    (require 'zenirc-notify)
    (require 'zenirc-popup)
    (require 'zenirc-signal)
    (require 'zenirc-stamp)
    (require 'zenirc-trigger)
    (require 'zenirc-yow)

    (delete-other-windows)))

;;(setq frame-title-format "IRC")
;;(zenirc-select "computer.eng.mindspring.net" "6667" "sj")


;;; Local Variables:
;;; mode:emacs-lisp
;;; sj/recompile-file:t
;;; End:
