(defun configure-line-numbers ()
    (global-display-line-numbers-mode))

(defun configure-column-numbers ()
    (setq column-number-mode t))

(defun configure-color-theme! ()
    (custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(ansi-color-faces-vector
      [default default default italic underline success warning error])
    '(ansi-color-names-vector
      ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
    '(custom-enabled-themes '(wheatgrass)))
   (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    ))

(configure-color-theme!)
(configure-line-numbers)
(configure-column-numbers)
