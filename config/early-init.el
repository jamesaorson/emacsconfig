(defun configure-weird-behaviors ()
  (setq vc-follow-symlinks t))

(defun install-packages ()
  (unless
      (and
       (package-installed-p 'magit)
       (package-installed-p 'clojure-mode))
    (require 'package)
    (add-to-list 'package-archives
		 '("melpa" . "https://melpa.org/packages/") t)
    (package-refresh-contents)
    (package-install 'magit)
    (package-install 'clojure-mode)))

(configure-weird-behaviors)
(install-packages)

