;; Disable menubar etc
(menu-bar-mode -1)

(show-paren-mode 1)


;; Setup tabs and spaces
(setq-default c-basic-offset 4
	      c-default-style "stroustrup"
	      tab-width 4
	      indent-tabs-mode t)

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)

;; Install my packages
(defvar soaboom-packages
  '(slime switch-window magit paredit tuareg merlin utop))

(require 'cl-lib)

(dolist (pkg soaboom-packages)
  (when (not (package-installed-p pkg))
	(package-install pkg)))

;; Keyboard shortcuts
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x d") 'delete-other-window)

;; Setup ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Setup common lisp environment
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-fancy slime-banner))

;; Setup OCaml
(setq auto-mode-alist
	  (append '(("\\.ml[ily]?$" . tuareg-mode)
				("\\.topml$" . tuareg-mode))
			  auto-mode-alist))

(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; Merlin
(add-to-list 'load-path (concat (substring (shell-command-to-string "opam config var share") 0 -1)
								"/emacs/site-lisp"))
(require 'merlin)

(add-hook 'tuareg-mode-hook 'merlin-mode)

(setq merlin-use-auto-complete-mode 'easy)
(setq merlin-error-after-save nil)

