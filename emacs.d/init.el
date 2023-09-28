;; Package Management
(require 'package)
(require 'eshell)

;;; Emacs Load Path
(add-to-list 'load-path "~/.emacs.d/packages/")

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
         (package-install package)))
   packages))

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar emacs-version-major (string-to-number (car (split-string emacs-version "\\."))))

;; Load manual packages
(require 'move-text)

;; Install community packages
(install-packages
 ;; [DOCS](https://github.com/auto-complete/auto-complete)
 'auto-complete
 'cuda-mode
 ;; [DOCS](https://github.com/jacobono/emacs-gradle-mode/tree/master)
 'gradle-mode
 ;; [DOCS](https://github.com/hcl-emacs/terraform-mode)
 'terraform-mode
 'yaml-mode
 )
(when (>= emacs-version-major 24)
  (install-packages
   'json-mode
   'xterm-color))
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
   'pdf-tools
   'treemacs
   'treemacs-all-the-icons
   'treemacs-icons-dired
   'treemacs-magit))
(when (>= emacs-version-major 27)
  (install-packages
   'markdown-mode))
;; [DOCS](https://github.com/emacsmirror/rainbow-mode)
(when (>= emacs-version-major 29)
  (install-packages
   'lsp-java
   'lsp-mode
   'rainbow-mode))

;; Configuration

(defun configure-antlr-mode ()
  (defun -configure-antlr-mode ()
    (when (and (stringp buffer-file-name)
               (string-match "\\.g4\\'" buffer-file-name))
      (antlr-mode)))

  (add-hook 'find-file-hook '-configure-antlr-mode))

(defun configure-column-mode ()
  (setq column-number-mode t))

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

(defun -unconfigure-line-mode-local ()
  "Source: https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/ - Disable line numbering in the current buffer"
  (display-line-numbers-mode -1))

(defun configure-move-text ()
  (move-text-default-bindings))

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

(defun configure-treemacs ()
  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once)
    :ensure nil)

  (use-package treemacs-magit
    :after (treemacs magit)
    :ensure nil))

(defun configure-tramp-mode ()
  "Source: https://www.emacswiki.org/emacs/TrampMode#h5o-4 - Configures tramp mode and fixes the shell defaults"
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(defun configure-weird-behaviors ()
  (setq vc-follow-symlinks t)
  (xterm-mouse-mode t)
  ;;(ac-config-default)
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (c-set-offset 'case-label 4 nil)
  (setq font-lock-maximum-decoration t))

(defun configure-xterm ()
  ;; BEGIN XTERM
  ;; Comint
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  ;; Eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-mode-hook
              (lambda () (progn
                           (setq xterm-color-preserve-properties t)
                           (setenv "TERM" "xterm-256color"))))

    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; Compilation
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

  ;; END XTERM
  )

(when (>= emacs-version-major 29)
  (configure-java)
  (configure-line-mode)
  (configure-pdf-mode))

(configure-antlr-mode)
(configure-column-mode)
(configure-hotkeys)
(configure-indent)
(configure-move-text)
(configure-tab-mode)
(configure-terraform-mode)
(configure-treemacs)
(configure-tex)
(configure-tramp-mode)
(configure-weird-behaviors)
(configure-xterm)

(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(manoj-dark))
 '(inhibit-startup-screen t)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

