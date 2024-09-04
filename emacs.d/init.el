;;; init.el --- Init for Emacs -*- lexical-binding: t; -*-

;; Author: James Orson <jamesaorson@gmail.com>
;; Maintainer: James Orson <jamesaorson@gmail.com>

;;; Commentary:
;; Initial startup improvements

;;; Code:

(require 'package)
(require 'eshell)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(fset 'yes-or-no-p 'y-or-n-p)     ;; changes all yes/no questions to y/n type

(ignore-errors (set-face-attribute 'default nil
                                   :font "FiraMono Nerd Font Mono"
                                   :height 100))

;;; Emacs Load Path
(add-to-list 'load-path "~/.emacs.d/packages/")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun install-packages (&rest packages)
  "Install list of PACKAGES.  Source: https://stackoverflow.com/a/10095853 - Assures every package is installed, ask for installation if it’s not, and return a list of installed packages (or nil for every skipped package)."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages))

(defvar +emacs-version-major+ (string-to-number (car (split-string emacs-version "\\."))))

;; Load manual packages
(require 'current-window-only)
(require 'dockerfile-mode)
(require 'move-text)

;; Install community packages
(install-packages
 'doom-themes
 'golden-ratio
 'indent-guide
 'restart-emacs
 'setup
 'simple-httpd
 'yaml-mode)
(when (>= +emacs-version-major+ 24)
  (install-packages
   'all-the-icons
   'amx
   'ido-completing-read+
   'ido-grid-mode
   'json-mode
   'toml-mode
   'which-key
   'xterm-color))
(when (>= +emacs-version-major+ 25)
  (install-packages
   'company
   'flycheck
   'graphviz-dot-mode
   'hl-todo
   ;; [DOCS](https://magit.vc/)
   'magit))
(when (>= +emacs-version-major+ 26)
  (install-packages
   ;; [DOCS](https://github.com/vedang/pdf-tools)
   'pdf-tools))
(when (>= +emacs-version-major+ 27)
  (install-packages
   'markdown-mode))
(when (>= +emacs-version-major+ 28)
  (install-packages
   'flycheck-eglot
   'geiser-guile))
;; [DOCS](https://github.com/emacsmirror/rainbow-mode)
(when (>= +emacs-version-major+ 29)
  (install-packages
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
(defun toggle-comment-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 4 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 4)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Return the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

;; Configuration

(defun configure-column-mode ()
  "Add column info to bar."
  (setq column-number-mode t))

(defun configure-company ()
  "Configure company mode."
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode))

(defun configure-current-window-only ()
  "Open new buffers in the current window, instead of continually splitting."
  (current-window-only-mode t))

(defun configure-dockerfile-mode ()
  "Enable dockerfile mode."
  (defun -configure-dockerfile-mode ()
    (when (and (stringp buffer-file-name)
               (string-match "\\Dockerfile\\'" buffer-file-name))
      (dockerfile-mode)))

  (add-hook 'find-file-hook '-configure-dockerfile-mode)
  (setq dockerfile-build-progress 'plain))

(defun configure-flycheck ()
  "Enable flycheck mode for basic IDE support in most languages."
  (global-flycheck-mode t))

(defun configure-hotkeys ()
  "Add various hotkeys that I use in my workflow."
  (global-set-key (kbd "<C-M-up>")    'windmove-up)
  (global-set-key (kbd "<C-M-down>")  'windmove-down)
  (global-set-key (kbd "<C-M-left>")  'windmove-left)
  (global-set-key (kbd "<C-M-right>") 'windmove-right))

(defun configure-ido ()
  "Enable ido mode."
  (ido-mode 1)
  (ido-grid-mode 1)
  (ido-everywhere 1)

  (ido-ubiquitous-mode 1)

  (amx-mode 1))

(defun configure-indent ()
  "Set up default indent."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq indent-line-function 'insert-tab))

(defun configure-line-mode ()
  "Display line numbers in gutter."
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  (global-hl-line-mode)
  ;; but not everywhere
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(defun configure-lisp ()
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(defun configure-move-text ()
  "Enable the ability to move text, row by row."
  (move-text-default-bindings))

(defun configure-pdf-mode ()
  "Configure PDF viewing settings."
  (defun -unconfigure-line-mode-local ()
    (display-line-numbers-mode -1))
  (add-hook 'pdf-view-mode-hook #'-unconfigure-line-mode-local)
  (pdf-tools-install))

(defun configure-geiser ()
  "Configure the version of scheme to use with geiser."
  (setq geiser-active-implementations '(guile)))

(defun configure-tab-mode ()
  "Disable tabs in the Emacs GUI."
  (global-tab-line-mode)
  (tab-bar-mode 1))

(defun configure-tex ()
  "Configure the tex commands to be used in rendering."
  (setq latex-run-command "pdflatex")
  (setq tex-start-options "-shell-escape "))

(defun configure-theme ()
  "Set the color theme."
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Enables transparency of terminal
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))
  
  (add-hook 'window-setup-hook 'on-after-init))

(defun configure-tree-sitter ()
  "Enable tree sitter grammars for installation via \\[treesit-install-language-grammar]."
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (commonlisp "https://github.com/theHamsta/tree-sitter-commonlisp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (org "https://github.com/milisims/tree-sitter-org")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (scheme "https://github.com/6cdh/tree-sitter-scheme")
          (toml "https://github.com/ikatyang/tree-sitter-toml")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode))))

(defun configure-tramp-mode ()
  "Fix tramp mode issues."
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(defun configure-which-key ()
  "Enable which key."
  (which-key-mode))

(defun configure-other ()
"Miscellaneous crap to configure."
  (global-hl-todo-mode t)
  (setq vc-follow-symlinks t)
  (xterm-mouse-mode nil)
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
        (exec-path-from-shell-initialize)))
  
  ;; Enable golden ratio windows
  (golden-ratio-mode 1)
  (indent-guide-global-mode))

(defun configure-xterm ()
  "Fix various xterm rendering issues."
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
  )

(when (>= +emacs-version-major+ 29)
  (configure-line-mode)
  (configure-pdf-mode))

(configure-column-mode)
(configure-company)
(configure-dockerfile-mode)
(configure-flycheck)
(configure-geiser)
(configure-hotkeys)
(configure-ido)
(configure-indent)
(configure-lisp)
(configure-move-text)
(configure-tab-mode)
(configure-tree-sitter)
(configure-tex)
(configure-tramp-mode)
(configure-which-key)
(configure-other)
(configure-xterm)

(configure-theme)

;; Interactive functions
(defun bootstrap-tree-sitter-grammars ()
  "Install tree sitter grammars from source.  treesit-language-source-alist contain the grammars to install."
  (interactive)
  (defun -install-grammar (grammar)
    (treesit-install-language-grammar grammar))
  (mapc #'-install-grammar (mapcar #'car treesit-language-source-alist)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;;; init.el ends here
