(defun configure-weird-behaviors ()
  (setq vc-follow-symlinks t))

(defun install-packages ()
  (when
      (eq (getenv "EMACS_INSTALL_PACKAGES") "1")
    (require 'package)
    (add-to-list 'package-archives
		 '("melpa" . "https://melpa.org/packages/") t)
    (package-refresh-contents)
    (package-install 'magit)))

(configure-weird-behaviors)
(install-packages)

