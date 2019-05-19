(custom-set-variables '(inhibit-startup-screen t))
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
  )
)
(add-hook 'after-make-frame-functions
  (lambda (frame)
    (let ((mode (if (display-graphic-p frame) 'light 'dark)))
      (set-frame-parameter frame 'background-mode mode)
      (set-terminal-parameter frame 'background-mode mode)
    )
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
  )
)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)

;; required themes
;; selected theme will live in custom
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package dracula-theme :ensure t)

(use-package rainbow-delimiters :ensure t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t      ; Don't delink hardlinks
      version-control t        ; User version numbers on backups
      delete-old-versions t    ; Automatically delete excess backups
      kept-new-versions 20     ; how many of the newest versions to keep
      kept-old-versions 5      ; and how many of the old
)

;(global-set-key (kbd "C-x g") 'helm-ag-project-root)
(global-set-key (kbd "C-x T") 'ansi-term)

(use-package company :ensure t)
(use-package company-tern :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-tern)

(use-package flycheck :ensure t)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq-default flycheck-temp-prefix ".flycheck")
(add-hook 'python-mode-hook
 (lambda ()
   (setq flycheck-python-pylint-executable "/home/dvadney/.virtualenvs/py3/bin/pylint")
))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x))
  :config
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x b") 'helm-buffers-list)
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
)
(use-package helm-projectile
  :ensure t
  :config
  (global-set-key (kbd "C-x C-p") 'helm-projectile))

(eval-after-load "org"
  '(require 'ox-md nil t))
