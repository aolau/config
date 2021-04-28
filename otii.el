(defvar *otii-core-root* default-directory)

(defun otii-core-build-root ()
  (concat *otii-core-root* "/build"))

(defun otii-core-build ()
  (interactive)
  (compile (concat "cd " (otii-core-build-root) " && make -j8")))

(defun otii-core-clean ()
  (interactive)
  (shell-command (concat "cd " (otii-core-build-root) " && make clean")))

(defun otii-core-git-status ()
  (interactive)
  (magit-status *otii-core-root*))

(global-set-key (kbd "C-c o b") 'otii-core-build)
(global-set-key (kbd "C-c o c") 'otii-core-clean)
(global-set-key (kbd "C-c o g") 'otii-core-git-status)
