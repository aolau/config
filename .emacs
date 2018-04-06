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
(defvar aolau-packages
  '(slime ac-slime magit paredit ivy counsel smex zenburn-theme afternoon-theme))

(require 'cl-lib)

(defun aolau-install-packages ()
  (package-refresh-contents)
  (dolist (pkg aolau-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;(aolau-install-packages)

;; Keyboard shortcuts
(global-set-key (kbd "C-x d") 'delete-other-window)

(windmove-default-keybindings)

;; Tags
(defun aolau-find-tag-default ()
  (interactive)
  (find-tag (find-tag-default)))

;; Qlik stuff
(defvar *qlik-engine-root* "/home/cmp/files/engine/")

(defun qlik-cd-engine-root ()
  (interactive)
  (cd *qlik-engine-root*))

(defun qlik-set-engine-root ()
  (interactive)
  (setq *qlik-engine-root* (read-directory-name "Engine root: " *qlik-engine-root*))
  (qlik-cd-engine-root))

(defun qlik-compile-engine ()
  (interactive)
  (and (qlik-cd-engine-root)
       (compile "make -k QlikMain CONFIG=RELEASE")))

(global-set-key (kbd "C-c c") 'qlik-compile-engine)

;; Ctags
(defun qlik-ctags-c++ ()
  (interactive)
  (let* ((ctags-root (read-directory-name "Tags root: " (concat *qlik-engine-root* "src/")))
         (ctags-file (concat ctags-root "TAGS")))
    (shell-command (concat "ctags -e -R --c++-kinds=+p --fields=+iaS --extra=+qf -f "
                           ctags-file
                           " "
                           ctags-root))
    (visit-tags-table ctags-file)))

(eval-after-load "etags"
  '(progn
     (ac-etags-setup)))

(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-,") 'etags-select-find-tag)


;; Rgrep
(eval-after-load "grep"
  '(grep-compute-defaults))

(defun qlik-rgrep ()
  (interactive)
  (let ((pattern (read-string "Pattern: " (if (region-active-p)
                                              (buffer-substring (mark) (point))
                                            (thing-at-point 'word)))))
    (rgrep pattern "*.h *.c *.cpp" *qlik-engine-root* nil)))

(global-set-key (kbd "M-/") 'qlik-rgrep)

(defun qlik-find-file ()
  (interactive)
  (let ((dir (read-directory-name "Find root: " *qlik-engine-root*)))
    (find-name-dired dir (read-string "Pattern: " ".*"))))

(global-set-key (kbd "M-\\") 'qlik-find-file)

(require 'paredit)

;; Paredit keys
(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)

(define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)

;; Ediff options
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Ivy
(ivy-mode 1)

;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Old M-x.
; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; Key bindings
;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

(global-set-key (kbd "M-;") 'auto-complete)

(setq ac-auto-show-menu nil)
(setq ac-auto-start nil)
(setq ac-delay 0)
(setq ac-quick-help-delay 1.0)

;; Setup Common Lisp
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-fancy slime-banner))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Setup theme
(load-theme 'afternoon)
