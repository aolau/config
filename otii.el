(defvar *otii-root* default-directory)
(defvar *otii-root-core* (concat *otii-root* "/core"))
(defvar *otii-root-core-build* (concat *otii-root-core* "/build"))
(defvar *otii-root-desktop* (concat  *otii-root* "/client/desktop"))
(defvar *otii-root-common* (concat  *otii-root* "/client/common"))
(defvar *otii-root-server* (concat  *otii-root* "/client/server"))
(defvar *otii-boost-root* "/home/andreas/code/boost_root")

(defun otii-build-core ()
  (interactive)
  (setenv "OTII_BOOST_ROOT" *otii-boost-root*)
  (compile (concat "cd " *otii-root-core* " && cmake --build build --config RelWithDebInfo")))

(defun otii-build-core-debug ()
  (interactive)
  (setenv "OTII_BOOST_ROOT" *otii-boost-root*)
  (compile (concat "OTII_BOOST_ROOT=" *otii-boost-root* " cd " *otii-root-core* " && cmake --build build --config Debug")))

(defun otii-build-desktop ()
  (interactive)
  (compile (concat "cd " *otii-root-desktop* " && tsc -b --pretty false")))

(defun otii-build-server ()
  (interactive)
  (compile (concat "cd " *otii-root-server* " && tsc -b --pretty false")))

(defun otii-desktop-lint ()
  (interactive)
  (compile (concat "cd " *otii-root-desktop* " && npm run lint")))

(defun otii-common-lint ()
  (interactive)
  (compile (concat "cd " *otii-root-common* " && npm run lint")))

(defun otii-git-status ()
  (interactive)
  (magit-status *otii-root*))

(global-set-key (kbd "C-c o b") 'otii-build-core)
(global-set-key (kbd "C-c o B") 'otii-build-core-debug)

(global-set-key (kbd "C-c o d") 'otii-build-desktop)
(global-set-key (kbd "C-c o s") 'otii-build-server)

(global-set-key (kbd "C-c o g") 'otii-git-status)
