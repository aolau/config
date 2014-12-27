; Disable menubar etc
(menu-bar-mode -1)

(show-paren-mode 1)

(global-set-key (kbd "C-x o") 'switch-window)

; Setup tabs and spaces
(setq-default c-basic-offset 4
	      c-default-style "stroustrup"
	      tab-width 4
	      indent-tabs-mode t)

; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

; Install my packages
(defvar soaboom-packages
  '(slime switch-window magit paredit))

(require 'cl-lib)

(dolist (pkg soaboom-packages)
  (when (not (package-installed-p pkg))
	(package-install pkg)))

; Setup common lisp environment
(setq inferior-lisp-program "sbcl")
(slime-setup  '(slime-repl slime-fancy slime-banner))
