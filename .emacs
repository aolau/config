; Disable menubar etc
(menu-bar-mode -1)

; Setup tabs and spaces
(setq-default c-basic-offset 4
	      c-default-style "stroustrup"
	      tab-width 4
	      indent-tabs-mode t)

; Setup common lisp environment
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")

(setq slime-lisp-implementations
     `((clisp ("/opt/local/bin/clisp"))))

(require 'slime)
(slime-setup  '(slime-repl slime-fancy slime-banner))
