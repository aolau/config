;; Disable menubar etc
(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(setq ring-bell-function 'ignore)

(show-paren-mode 1)
(setq inhibit-splash-screen t)

(global-hl-line-mode 1)

(setq x-super-keysym 'meta)

(global-unset-key (kbd "C-z"))

(put 'upcase-region 'disabled nil)

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Font
(cond ((string-equal system-type "darwin") t)
      ((string-equal system-type "windows-nt") (set-face-attribute 'default nil :family "Consolas" :height 120))
      (t (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height 120)))

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
  '(company slime slime-company magit paredit ivy counsel counsel-projectile smex zenburn-theme afternoon-theme yasnippet cmake-mode typescript-mode eglot elpy))

(require 'cl-lib)

(defun aolau-install-packages ()
;  (package-refresh-contents)
  (dolist (pkg aolau-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;(aolau-install-packages)

(defun kill-buffer-other-window ()
  (interactive)
  (save-window-excursion
    (other-window 1)
    (kill-buffer)))

;; Keyboard shortcuts
(global-set-key (kbd "C-x d") 'kill-buffer-other-window)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-c g") 'magit-status)

(windmove-default-keybindings)

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
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Auto complete with company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay nil)

(global-set-key (kbd "M-;") 'company-complete)

;; yasnippet
(yas-global-mode)

;; org-mode
(electric-indent-mode -1)

;; C++
(require 'eglot)
(add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1) (eldoc-mode -1)))
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(setq eglot-ignored-server-capabilites (quote (:documentHighlightProvider)))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Setup Common Lisp
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-fancy slime-banner slime-company))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq search-upper-case t)

;; Setup theme
(load-theme 'afternoon)
(set-face-attribute 'mode-line nil :height 130 :family "DejaVu Sans Mono")

(require 'subr-x)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)

;; For elpy
(setq elpy-rpc-python-command "python3.7")
;; For interactive shell
(setq python-shell-interpreter "python3.7"
      python-shell-interpreter-args "-i")

(defun load-if-exists (filename)
  (if (file-exists-p filename)
      (load filename)))

(load-if-exists "~/otii.el")
