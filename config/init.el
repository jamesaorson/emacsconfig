(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun configure-column-mode ()
  (setq column-number-mode t))

(defun configure-hotkeys ()
  (global-set-key (kbd "<C-M-up>")    'windmove-up)
  (global-set-key (kbd "<C-M-down>")  'windmove-down)
  (global-set-key (kbd "<C-M-left>")  'windmove-left)
  (global-set-key (kbd "<C-M-right>") 'windmove-right))

(defun configure-line-mode ()
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative)
  (global-hl-line-mode))

(defun -unconfigure-line-mode-local ()
  "Source: https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/ - Disable line numbering in the current buffer"
  (display-line-numbers-mode -1))

(defun configure-pdf-mode ()
  "Source: https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/"
  (add-hook 'pdf-view-mode-hook #'-unconfigure-line-mode-local))

(defun configure-tex ()
  (setq latex-run-command "pdflatex"))

(defun configure-tramp-mode ()
  "Source: https://www.emacswiki.org/emacs/TrampMode#h5o-4 - Configures tramp mode and fixes the shell defaults"
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(defun configure-weird-behaviors ()
  (setq vc-follow-symlinks t)
  (xterm-mouse-mode t)
  (menu-bar-mode -1))

(defun install-packages (&rest packages)
  "Source: https://stackoverflow.com/a/10095853 - Assures every package is installed, ask for installation if it’s not, and returns a list of installed packages (or nil for every skipped package)"
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
(configure-hotkeys)
(configure-pdf-mode)
(configure-tex)
(configure-tramp-mode)
(configure-weird-behaviors)
;; more weird behaviors
(defalias 'yes-or-no-p 'y-or-n-p)
(install-packages
 ;; [DOCS](https://github.com/auto-complete/auto-complete)
 'auto-complete
 ;; [DOCS](https://github.com/clojure-emacs/cider)
 'cider
 ;; [DOCS](https://magit.vc/) 
 'magit
 'markdown-mode
 ;; [DOCS](https://github.com/vedang/pdf-tools)
 'pdf-tools
 ;; [DOCS](https://github.com/emacsmirror/rainbow-mode)
 'rainbow-mode
 ;; [DOCS](https://github.com/hcl-emacs/terraform-mode)
 'terraform-mode
 )
;; auto-complete
(ac-config-default)
;; pdf-tools
(pdf-tools-install)
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
   '(## clojure-mode tabbar session pod-mode muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode company color-theme-modern browse-kill-ring boxquote bm bar-cursor apache-mode magit))
 '(terraform-format-on-save t)
 '(terraform-indent-level 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
