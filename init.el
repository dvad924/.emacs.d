;;; Commentary:
;;; Code:
(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
;; Config
(require 'org)
(org-babel-load-file  "~/.emacs.d/configuration.org")
(load custom-file 'noerror)
