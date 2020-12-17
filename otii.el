(defvar *otii-root* default-directory)

(defun otii-build-dir ()
  (concat *otii-root* "build"))
  
(defun otii-build ()
  (interactive)
  (cd (otii-build-dir))
  (compile "make -j8"))

(defun otii-debug ()
  (interactive)
  (cd (otii-build-dir))
  (gdb "gdb -i=mi otii"))

(global-set-key (kbd "C-c d") 'otii-debug)

(defun otii-git ()
  (interactive)
  (magit-status)
  (cd *otii-root*))

(global-set-key (kbd "C-c g") 'otii-git)

(defun otii-c++-tags ()
  (interactive)
  (cd *otii-root*)
  (shell-command "ctags -e -R --c++-kinds=+p --fields=+iaS --extra=+q ."))

(global-set-key (kbd "C-c c") 'otii-build)
(global-set-key (kbd "C-c k") 'kill-compilation)

(defun otii-current-selection ()
  (if (use-region-p)
      (buffer-substring (mark) (point))
    (thing-at-point 'word 'no-properties)))

(eval-after-load "grep"
  '(add-to-list 'grep-find-ignored-directories "third_party"))

(add-hook 'c++-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(defun otii-grep ()
  (interactive)
  (rgrep (otii-current-selection) "*.h *.cpp" *otii-root*))

(global-set-key (kbd "M-/") 'otii-grep)
