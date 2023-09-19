;; Package Management

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar emacs-version-major (string-to-number (car (split-string emacs-version "\\."))))

(install-packages
 ;; [DOCS](https://github.com/auto-complete/auto-complete)
 'auto-complete
 ;; [DOCS](https://github.com/jacobono/emacs-gradle-mode/tree/master)
 'gradle-mode
 ;; [DOCS](https://github.com/hcl-emacs/terraform-mode)
 'terraform-mode
 'yaml-mode
 )
(when (>= emacs-version-major 24)
  (install-packages
   'dired-quick-sort
   'json-mode))
(when (>= emacs-version-major 25)
  (install-packages
   'graphviz-dot-mode
   'kubernetes
   ;; [DOCS](https://magit.vc/) 
   'magit
   'tree-sitter
   'tree-sitter-langs))
(when (>= emacs-version-major 26)
  (install-packages
   ;; [DOCS](https://github.com/clojure-emacs/cider)
   'cider
   ;; [DOCS](https://github.com/vedang/pdf-tools)
   'pdf-tools)
   'treemacs)
(when (>= emacs-version-major 27)
  (install-packages
   'markdown-mode))
;; [DOCS](https://github.com/emacsmirror/rainbow-mode)
(when (>= emacs-version-major 29)
  (install-packages
   'lsp-java
   'lsp-mode
   'rainbow-mode))

;; Custom Modes

(setq tiger-keywords (regexp-opt '(
                                   "array"
                                   "begin"
                                   "break"
                                   "do"
                                   "else"
                                   "end"
                                   "enddo"
                                   "endif"
                                   "float"
                                   "for"
                                   "function"
                                   "if"
                                   "int"
                                   "let"
                                   "of"
                                   "program"
                                   "return"
                                   "static"
                                   "then"
                                   "to"
                                   "type"
                                   "var"
                                   "while")
                                 t))
;; TODO: Write an emacs mode for tiger - https://www.emacswiki.org/emacs/ModeTutorial
(defun tiger-mode ()
  ())

;; Configuration

(defun configure-antlr-mode ()
  (defun -configure-antlr-mode ()
    (when (and (stringp buffer-file-name)
               (string-match "\\.g4\\'" buffer-file-name))
      (antlr-mode)))

  (add-hook 'find-file-hook '-configure-antlr-mode))

(defun configure-column-mode ()
  (setq column-number-mode t))

(defun configure-dired ()
  (require 'dired-quick-sort)
  (dired-quick-sort-setup))

(defun configure-hotkeys ()
  (global-set-key (kbd "<C-M-up>")    'windmove-up)
  (global-set-key (kbd "<C-M-down>")  'windmove-down)
  (global-set-key (kbd "<C-M-left>")  'windmove-left)
  (global-set-key (kbd "<C-M-right>") 'windmove-right))

(defun configure-indent ()
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab))

(defun configure-java ()
  (require 'lsp-java)
  (add-hook 'java-mode-hook #'lsp))

(defun configure-line-mode ()
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative)
  (global-hl-line-mode))

(defun configure-lsp ()
  ())

(defun -unconfigure-line-mode-local ()
  "Source: https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/ - Disable line numbering in the current buffer"
  (display-line-numbers-mode -1))

(defun configure-pdf-mode ()
  "Source: https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/"
  (add-hook 'pdf-view-mode-hook #'-unconfigure-line-mode-local)
  (pdf-tools-install))

(defun configure-tab-mode ()
  (global-tab-line-mode)
  (tab-bar-mode 1))

(defun configure-terraform-mode ()
  (add-hook 'terraform-mode-hook #'outline-minor-mode))

(defun configure-tex ()
  (setq latex-run-command "pdflatex"))

(defun configure-tiger-mode ()
  (defun -configure-tiger-mode ()
    (when (and (stringp buffer-file-name)
               (string-match "\\.tiger\\'" buffer-file-name))
      (tiger-mode)))

  (add-hook 'find-file-hook '-configure-tiger-mode))

(defun configure-tramp-mode ()
  "Source: https://www.emacswiki.org/emacs/TrampMode#h5o-4 - Configures tramp mode and fixes the shell defaults"
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(defun configure-weird-behaviors ()
  (setq vc-follow-symlinks t)
  (xterm-mouse-mode t)
  (menu-bar-mode -1)
  (ac-config-default)
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (c-set-offset 'case-label 4 nil)
  (setq font-lock-maximum-decoration t))

(when (>= emacs-version-major 29)
  (configure-java)
  (configure-line-mode)
  (configure-lsp)
  (configure-pdf-mode))

(configure-antlr-mode)
(configure-column-mode)
(configure-dired)
(configure-hotkeys)
(configure-indent)
(configure-tab-mode)
(configure-terraform-mode)
(configure-tex)
(configure-tramp-mode)
(configure-weird-behaviors)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(manoj-dark))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(treemacs)
(other-window 1)

