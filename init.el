;;; Commentary:
;;; Code:
(package-initialize)
;; (setq custom-file "~/.emacs.d/custom.el")
;; Config
(require 'org)
(org-babel-load-file  "~/.emacs.d/configuration.org")
;; (load custom-file 'noerror)
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (dumb-jump use-package terraform-mode rainbow-delimiters magit helm-projectile flycheck dracula-theme doom-themes company-tern company-restclient company-go color-theme-sanityinc-tomorrow all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
