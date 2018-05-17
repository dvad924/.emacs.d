;;; package --- Summary
;;; commentary:
;;package repos

;;; Code:
(require 'package)
;;; ENABLE melpa
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;(load "~/.emacs.d/installed.el")
;;; GLOBAL CONFIG
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;; c/c++ config
(load-file "~/.emacs.d/emacs-c-ide-demo/init.el")

;;; mu4e config
(load-file "~/.emacs.d/mu4e-config.el")
;;; Global Key Configs
(global-set-key (kbd "C-x g") 'helm-ag-project-root)
(global-set-key (kbd "C-x T") 'ansi-term)

;;; SQL configs
(load-file "~/.emacs.d/dbconfig.el")



;; ORG CONFIG
(require 'org)
(setq org-agenda-files '("~/notes/"))
(require 'cl)
(load-file "~/.emacs.d/org-learn.el")
(load-file "~/.emacs.d/org-drill.el")
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(defun build-card-vocab ()
  "Takes User through a set of prompts to build a card template."
  (interactive)
  (insert "** vocab  :drill:\n"
	  ":PROPERTIES:\n"
	  ":DRILL_CARD_TYPE: twosided\n"
	  ":END:\n"
	  "Translate this word\n\n"
	  "*** Chinese\n"
	  (read-string "Chinese Chars: ") "\n"
	  "*** English\n"
	  (read-string "English Meaning: ") "\n"
	  "*** Example\n"
	  (read-string "Example Usage: ") "\n"))

(defun build-card-pinyin ()
  "Takes User through a set of prompts to build a pronounciation card."
  (interactive)
  (insert "** pinyin  :drill:\n"
	  "What is the pinyin of "
	  (read-string "Chinese Char: ") "?\n"	  	  
	  "*** Answer\n"
	  (read-string "Pinyin: ") "\n"))


;;python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

;; TABS
;;(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq indent-tabs-mode t)
;; line numbers
;; (global-linum-mode 1)

;;save all backups to particular directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t      ; Don't delink hardlinks
      version-control t        ; User version numbers on backups
      delete-old-versions t    ; Automatically delete excess backups
      kept-new-versions 20     ; how many of the newest versions to keep
      kept-old-versions 5      ; and how many of the old
)


(require 'package)
(setq package-enable-at-startup nil) ;;avoid initializing twice
(package-initialize)
;;enabling projectile


;;enable flex for ido
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)

;; ;;disable ido faces to see flx highlights
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)
;; (setq flx-ido-threshold 5000)

;;auto complete with company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-tern)

;;use web-mode for .jsx files
;;(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.react.js$" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.js" . web-mode))

;; javascript web-mode hack
(add-hook 'web-mode-hook
	  (lambda ()
	    ;; short circuit js mode and do everything in jsx-mode
	    (if (equal web-mode-content-type "javascript")
		(web-mode-set-content-type "jsx")
	      (message "now set to: %s" web-mode-content-type))))

	     
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;;customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")


;;Javascript
(add-to-list 'auto-mode-alist '("//.json$" . js-mode))
(load-file "~/.emacs.d/emacs-js/emacs-js.el")
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)

 

;;flymake for on the fly checking
;;spell checking for latex
;(setq ispell-program-name "aspell")
;(setq ispell-dictionary "english")
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
;; enable flycheck by default
(add-hook 'after-init-hook#'global-flycheck-mode)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)))
  

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (let ((mode (if (display-graphic-p frame) 'light 'dark)))
	      (set-frame-parameter frame 'background-mode mode)
	      (set-terminal-parameter frame 'background-mode mode))
	            ;;; Disable gui BS
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (scroll-bar-mode -1)
    ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#515151" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#cccccc"))
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("e297f54d0dc0575a9271bb0b64dad2c05cff50b510a518f5144925f627bb5832" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(fci-rule-color "#515151")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill))
 '(package-selected-packages
   '(csv-mode haskell-mode 0xc pdf-tools color-theme-sanityinc-tomorrow iedit anzu ws-butler dtrt-indent clean-aindent-mode volatile-highlights helm-gtags helm-projectile helm-swoop zygospore use-package docker-api docker-compose-mode docker-tramp rainbow-identifiers ob-browser ob-restclient ob-sql-mode org-beautify-theme multi-term company company-php company-restclient restclient undo-tree dracula-theme cargo magit helm-ag ag rainbow-blocks org org-drill-table company-go go-mode xref-js2 clojure-mode clojurescript-mode buffer-move nodejs-repl markdown-mode dockerfile-mode csharp-mode websocket web-mode web-beautify solarized-theme request racer pylint protobuf-mode projectile nvm multi-web-mode json-mode js-comint flycheck-rust flx-ido elpy cuda-mode company-web company-tern company-lua company-cmake company-c-headers company-anaconda color-theme-solarized color-theme-sanityinc-solarized cm-mode cl-generic auto-complete-auctex auctex ac-js2 ac-anaconda))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(projectile-mode t nil (projectile))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tramp-syntax 'default nil (tramp))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "PfEd" :slant normal :weight normal :height 140 :width normal)))))
