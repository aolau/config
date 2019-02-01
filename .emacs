;; Disable menubar etc
(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(setq ring-bell-function 'ignore)

(show-paren-mode 1)
(setq inhibit-splash-screen t)

(global-hl-line-mode 1)

;; Font
(cond ((string-equal system-type "darwin") t)
      ((string-equal system-type "windows-nt") (set-face-attribute 'default nil :family "Consolas" :height 120))
      (t (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height 100)))

;; Git binary
(when (string-equal system-type "windows-nt")
  (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
  (setq ediff-diff-program "C:/Program Files/Git/bin/diff.exe"))

;; Setup backup
(setq backup-directory-alist '(("." . "~/.emacs_bkp")))
(setq backup-by-copying t)

(setq auto-save-default nil)

;; Setup tabs and spaces
(setq-default c-basic-offset 4
              c-default-style "stroustrup"
              tab-width 4
              indent-tabs-mode nil
              lua-indent-level 2)

;; C++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Backspace
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "<f1>") 'help-command)

;; Split behavior
(setq split-height-threshold nil)

;; Forward word
(require 'misc)
(global-set-key (kbd "M-f") 'forward-to-word)

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Install my packages
(defvar aolau-packages
  '(slime ac-slime magit paredit ivy counsel counsel-projectile smex zenburn-theme afternoon-theme yasnippet ac-etags))

(require 'cl-lib)

(defun aolau-install-packages ()
  (package-refresh-contents)
  (dolist (pkg aolau-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;(aolau-install-packages)

;; Keyboard shortcuts
(global-set-key (kbd "C-x d") 'delete-other-window)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-c s") 'projectile-find-other-file)

(windmove-default-keybindings)


(eval-after-load "etags"
  '(progn
     (ac-etags-setup)))

(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-,") 'etags-select-find-tag)

;; Rgrep
(eval-after-load "grep"
  '(grep-compute-defaults))

(require 'paredit)

;; Paredit keys
(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)

(define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)

;; Ediff options
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Ivy and counsel
(ivy-mode 1)
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Old M-x.
; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; Key bindings
;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

(global-set-key (kbd "C-c r") 'revert-buffer)

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

(global-set-key (kbd "M-;") 'auto-complete)

(setq ac-auto-show-menu nil)
(setq ac-auto-start nil)
(setq ac-delay 0)
(setq ac-quick-help-delay 1.0)

;; yasnippet
(yas-global-mode)

;; Setup Common Lisp
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-fancy slime-banner))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq search-upper-case t)

;; Setup theme
(load-theme 'afternoon)

(load "~/files/scripts/qlik.el")
