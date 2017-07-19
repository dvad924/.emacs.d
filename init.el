;;; package --- Summary
;;; commentary:
;;package repos

;;; Code:
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(load-theme 'manoj-dark t)
;; org
(require 'org)
(require 'cl)
(load-file "~/.emacs.d/org-learn.el")
(load-file "~/.emacs.d/org-drill.el")
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/meetings.org"))
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
(setq indent-tabs-mode nil)
;; line numbers
(global-linum-mode 1)

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
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;;disable ido faces to see flx highlights
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq flx-ido-threshold 5000)

;;auto complete with company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-tern)

;;use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.react.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js" . web-mode))

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

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)
;; for better jsx syntax-highlighting in web-mode



;;Javascript
(add-to-list 'auto-mode-alist '("//.json$" . js-mode))
(load-file "~/.emacs.d/emacs-js/emacs-js.el")
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)

 
;;AucTex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
;; Use Skim as viewer, enable source <-> PDF sync
;;make latexmk available via C-c C-c
;;Note: SyncTeX is setup via ~/.latexmkrc
(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
				:help "Run latexmk on file")
			      TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b")))

;;flymake for on the fly checking
;;spell checking for latex
;(setq ispell-program-name "aspell")
;(setq ispell-dictionary "english")
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
;; enable flycheck by default
(add-hook 'after-init-hook#'global-flycheck-mode)


(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (let ((mode (if (display-graphic-p frame) 'light 'dark)))
	      (set-frame-parameter frame 'background-mode mode)
	      (set-terminal-parameter frame 'background-mode mode))
	                (enable-theme 'solarized)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (rainbow-blocks rainbow-delimiters org org-drill-table company-go go-mode xref-js2 clojure-mode clojurescript-mode buffer-move nodejs-repl markdown-mode dockerfile-mode csharp-mode websocket web-mode web-beautify solarized-theme request racer pylint protobuf-mode projectile nvm multi-web-mode json-mode js-comint flycheck-rust flx-ido elpy cuda-mode company-web company-tern company-lua company-cmake company-c-headers company-anaconda color-theme-solarized color-theme-sanityinc-solarized cm-mode cl-generic auto-complete-auctex auctex ac-js2 ac-anaconda)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
