;;; early-init.el --- Early init for Emacs -*- lexical-binding: t; -*-

;; Author: James Orson <jamesaorson@gmail.com>
;; Maintainer: James Orson <jamesaorson@gmail.com>

;;; Commentary:
;; Initial startup improvements

;;; Code:

;; improve startup time by pausing garbage collection during init
(setq gc-cons-threshold most-positive-fixnum)

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)))
                            ))

;;; -*- no-byte-compile: t -*-
(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/packages/")
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;;; early-init.el ends here

