;; Package Management
(require 'package)
(require 'eshell)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(fset 'yes-or-no-p 'y-or-n-p)     ;; changes all yes/no questions to y/n type

;; (set-face-attribute 'default nil :font "FiraMono Nerd Font")

;;; Emacs Load Path
(add-to-list 'load-path "~/.emacs.d/packages/")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
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

(defvar emacs-version-major (string-to-number (car (split-string emacs-version "\\."))))

;; Load manual packages
(require 'move-text)
(require 'current-window-only)

;; Install community packages
(install-packages
 ;; [DOCS](https://github.com/auto-complete/auto-complete)
 'auto-complete
 'cuda-mode
 'dumb-jump
 ;; [DOCS](https://github.com/jacobono/emacs-gradle-mode/tree/master)
 'gradle-mode
 'setup
 'slime
 ;; [DOCS](https://github.com/hcl-emacs/terraform-mode)
 'terraform-mode
 'yaml-mode
 )
(when (>= emacs-version-major 24)
  (install-packages
   'json-mode
   'amx
   'ido-completing-read+
   'ido-grid-mode
   'xterm-color))
(when (>= emacs-version-major 25)
  (install-packages
   'graphviz-dot-mode
   'hl-todo
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

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; shell scripts
(setq-default sh-basic-offset 2
              sh-indentation 2)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

;; Configuration

(defun configure-antlr-mode ()
  (defun -configure-antlr-mode ()
    (when (and (stringp buffer-file-name)
               (string-match "\\.g4\\'" buffer-file-name))
      (antlr-mode)))

  (add-hook 'find-file-hook '-configure-antlr-mode))

(defun configure-column-mode ()
  (setq column-number-mode t))

(defun configure-current-window-only ()
  (current-window-only-mode t))

(defun configure-hotkeys ()
  (global-set-key (kbd "<C-M-up>")    'windmove-up)
  (global-set-key (kbd "<C-M-down>")  'windmove-down)
  (global-set-key (kbd "<C-M-left>")  'windmove-left)
  (global-set-key (kbd "<C-M-right>") 'windmove-right))

(defun configure-ido ()
  (ido-mode 1)
  (ido-grid-mode 1)
  (ido-everywhere 1)

  (ido-ubiquitous-mode 1)

  (amx-mode 1))

(defun configure-indent ()
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab))

(defun configure-java ()
  (require 'lsp-java)
  (add-hook 'java-mode-hook #'lsp))

(defun configure-line-mode ()
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  (global-hl-line-mode)
  ;; but not everywhere
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(defun -unconfigure-line-mode-local ()
  "Source: https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/ - Disable line numbering in the current buffer"
  (display-line-numbers-mode -1))

(defun configure-move-text ()
  (move-text-default-bindings))

(defun configure-pdf-mode ()
  "Source: https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/"
  (add-hook 'pdf-view-mode-hook #'-unconfigure-line-mode-local)
  (pdf-tools-install))

(defun configure-slime ()
  (setq inferior-lisp-program "sbcl"))

(defun configure-tab-mode ()
  (global-tab-line-mode)
  (tab-bar-mode 1))

(defun configure-terraform-mode ()
  (add-hook 'terraform-mode-hook #'outline-minor-mode))

(defun configure-tex ()
  (setq latex-run-command "pdflatex"))

(defun configure-treemacs ()
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil)

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

(defun configure-other ()
  (global-hl-todo-mode t)
  (setq vc-follow-symlinks t)
  (xterm-mouse-mode nil)
  (ac-config-default)
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (c-set-offset 'case-label 4 nil)
  (setq font-lock-maximum-decoration t)

  (delete-selection-mode 1)

  ;; SOURCE: https://github.com/flyingmachine/emacs-for-clojure
  (tooltip-mode -1)                 ;; disable tooltips
  (tool-bar-mode -1)
  (if (display-graphic-p)
      (progn
        (scroll-bar-mode -1)))
  (blink-cursor-mode 0)             ;; turn off blinking cursor. distracting!
  (setq create-lockfiles nil)       ;; no need for ~ files when editing
  (setq inhibit-startup-message t)  ;; go straight to scratch buffer on startup
  (setq ring-bell-function 'ignore) ;; turn off audible bell

  (put 'upcase-region 'disabled nil)
  ;; show full path in title bar
  (setq-default frame-title-format "%b (%f)")

  ;; initial frame height and width
  (add-to-list 'default-frame-alist '(height . 45))
  (add-to-list 'default-frame-alist '(width . 100))

  ;; increase font size for better readability
  (set-face-attribute 'default nil :height 140)

  ;; on a Mac, don't pop up font menu
  (when (string-equal system-type "darwin") 'ok
        (global-set-key (kbd "s-t") '(lambda () (interactive))))

  ;; These settings relate to how emacs interacts with your operating system
  (setq ;; makes killing/yanking interact with the clipboard
   x-select-enable-clipboard t

   ;; Save clipboard strings into kill ring before replacing them.
   ;; When one selects something in another program to paste it into Emacs,
   ;; but kills something in Emacs before actually pasting it,
   ;; this selection is gone unless this variable is non-nil
   save-interprogram-paste-before-kill t

   ;; Shows all options when running apropos. For more info,
   ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
   apropos-do-all t

   ;; Mouse yank commands yank at point instead of at click.
   mouse-yank-at-point t)

  ;; Sets up exec-path-from shell
  ;; https://github.com/purcell/exec-path-from-shell
  (setup 
   (when (memq window-system '(mac ns))
     (:package exec-path-from-shell)
     (exec-path-from-shell-initialize))))

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
(configure-ido)
(configure-indent)
(configure-move-text)
(configure-slime)
(configure-tab-mode)
(configure-terraform-mode)
(configure-treemacs)
(configure-tex)
(configure-tramp-mode)
(configure-other)
(configure-xterm)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(auto-complete yaml-mode xterm-color treemacs-magit
                   treemacs-icons-dired treemacs-all-the-icons
                   tree-sitter-langs terraform-mode slime setup
                   rainbow-mode pdf-tools lsp-java kubernetes
                   json-mode ido-grid-mode ido-completing-read+
                   hl-todo graphviz-dot-mode gradle-mode dumb-jump
                   cuda-mode cider amx)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-background-color "#333333")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

