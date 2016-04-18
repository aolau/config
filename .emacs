;; Disable menubar etc
(menu-bar-mode -1)
(show-paren-mode 1)

;; Setup backup
(setq backup-directory-alist '(("." . "~/.emacs_bkp")))
(setq backup-by-copying t)

;; Setup tabs and spaces
(setq-default c-basic-offset 4
              c-default-style "stroustrup"
              tab-width 4
              indent-tabs-mode nil
              lua-indent-level 2)

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Install my packages
(defvar soaboom-packages
  '(slime ac-slime switch-window magit paredit tuareg merlin utop))

(require 'cl-lib)

(defun soaboom-install-packages ()
  (package-refresh-contents)
  (dolist (pkg soaboom-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;(soaboom-install-packages)

;; Keyboard shortcuts
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x d") 'delete-other-window)

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

;; Ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; Key bindings
;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; nodejs
; (global-set-key (kbd "C-c b") 'nodejs-repl-send-buffer)
; (global-set-key (kbd "C-c q") 'nodejs-repl-quit-or-cancel)


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

;; Setup Python
(setq elpy-rpc-python-command "python3")
(elpy-enable)
