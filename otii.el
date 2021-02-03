(defvar *otii-core-root* (concat default-directory "/core"))

(defun otii-core-build-root ()
  (concat *otii-core-root* "/build"))

(defun otii-core-build ()
  (interactive)
  (and (cd (otii-core-build-root))
       (compile "ninja")))

(defun otii-core-clean ()
  (interactive)
  (and (cd (otii-core-build-root))
       (shell-command "ninja clean")))
  
(global-set-key (kbd "C-c o b") 'otii-core-build)
(global-set-key (kbd "C-c o c") 'otii-core-clean)
