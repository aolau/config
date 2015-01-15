; Disable menubar etc
(menu-bar-mode -1)

(show-paren-mode 1)

; Mode-line
(setq-default mode-line-format
			  `((buffer-file-name (:eval (abbreviate-file-name buffer-file-name) "%b%"))
				"%4l"
				(vc-mode (:eval (concat " [" vc-mode " ]")))
				" ( %m )"
				" " mode-line-modified))

; Setup tabs and spaces
(setq-default c-basic-offset 4
	      c-default-style "stroustrup"
	      tab-width 4
	      indent-tabs-mode t)

; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

; Install my packages
(defvar soaboom-packages
  '(slime switch-window magit paredit))

(require 'cl-lib)

(dolist (pkg soaboom-packages)
  (when (not (package-installed-p pkg))
	(package-install pkg)))

; Keyboard shortcuts
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x d") 'delete-other-window)

; Setup ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; Setup common lisp environment
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-fancy slime-banner))
