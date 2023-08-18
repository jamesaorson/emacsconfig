(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun configure-weird-behaviors ()
  (setq vc-follow-symlinks t))

(defun install-packages (&rest packages)
  "Assure every package is installed, ask for installation if it’s not. Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(configure-weird-behaviors)
;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(unless package-archive-contents
  (package-refresh-contents))
(install-packages 'clojure-mode 'magit 'rainbow-mode)

