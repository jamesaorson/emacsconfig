(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun configure-column-mode ()
  (setq column-number-mode t))

(defun configure-line-mode ()
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative)
  (global-hl-line-mode))

(defun configure-starting-windows ()
  (split-window-right)
  (magit)
  (split-window-below)
  (other-window 1)
  (term "/bin/zsh")
  (other-window 1)
  (enlarge-window-horizontally 20))

(defun configure-tramp-mode ()
  ;; tramp mode ssh fix
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

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

(configure-line-mode)
(configure-column-mode)
(configure-tramp-mode)
(configure-weird-behaviors)
(install-packages
 ;; [DOCS](https://github.com/auto-complete/auto-complete)
 'auto-complete
 ;; [DOCS](https://github.com/clojure-emacs/cider)
 'cider
 ;; [DOCS](https://magit.vc/) 
 'magit
 ;; [DOCS](https://github.com/emacsmirror/rainbow-mode)
 'rainbow-mode
 ;; [DOCS](https://github.com/hcl-emacs/terraform-mode)
 'terraform-mode
 )
;; auto-complete
(ac-config-default)
;; terraform-mode
(add-hook 'terraform-mode-hook #'outline-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(manoj-dark))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(clojure-mode tabbar session pod-mode muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode company color-theme-modern browse-kill-ring boxquote bm bar-cursor apache-mode magit))
 '(terraform-format-on-save t)
 '(terraform-indent-level 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(configure-starting-windows)

