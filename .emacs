;; Disable menubar etc
(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (load-theme 'deeper-blue))

(setq ring-bell-function 'ignore)

(show-paren-mode 1)
(setq inhibit-splash-screen t)

(if (not (string-equal system-type "darwin"))
    (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height 160))
                    
;; Setup backup
(setq backup-directory-alist '(("." . "~/.emacs_bkp")))
(setq backup-by-copying t)

;; Setup tabs and spaces
(setq-default c-basic-offset 4
              c-default-style "stroustrup"
              tab-width 4
              indent-tabs-mode nil
              lua-indent-level 2)

;; Backspace
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "<f1>") 'help-command)

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Install my packages
(defvar soaboom-packages
  '(slime ac-slime ace-window magit paredit))

(require 'cl-lib)

(defun soaboom-install-packages ()
  (package-refresh-contents)
  (dolist (pkg soaboom-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;(soaboom-install-packages)

;; Keyboard shortcuts
(global-set-key (kbd "C-x d") 'delete-other-window)

(windmove-default-keybindings)

(defun soaboom-find-tag-default ()
  (interactive)
  (find-tag (find-tag-default)))

(global-set-key (kbd "M-.") 'soaboom-find-tag-default)

(require 'paredit)

;; Paredit keys
(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-[") 'paredit-forward-barf-sexp)

(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-]") 'paredit-backward-barf-sexp)

;; Ediff options
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; Key bindings
;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

; Sane ac delay times
(setq ac-delay 0.5)
(setq ac-quick-help-delay 2.0)

;; Setup Common Lisp
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-fancy slime-banner))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
