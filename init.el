;;==========================================================
;;      GENERAL SETUP
;;==========================================================

;; speed up start
(setq gc-cons-threshold 100000000)			; provide more memory

;; don't show startup message
(setq inhibit-startup-message t)

;; set home directory
(setq default-directory (concat (getenv "HOME") "/"))

;; set path to settings
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)

;; load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; set up appearance early
(require 'appearance)

;; character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
;; treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE 
(set-clipboard-coding-system 'utf-16le-dos)

;; write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "temp")))))

;; save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;;==========================================================
;;      PACKAGE MANAGEMENT
;;==========================================================

(when (>= emacs-major-version 24)
  (require 'package)
  ;; (add-to-list 'package-archives
	;;        '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))
;; list the packages you want
(setq package-list '(async 							; paradox needs async
										 paradox
										 use-package
                     ))
;; ;; fetch the list of packages available
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; install the missing packages (and refresh the package list if necessary)
(setq package-list-refreshed nil)
(dolist (package package-list)
  (unless (package-installed-p package)
		(if package-list-refreshed 				; package list already refreshed?
				(package-install package)				; package list already refreshed!
			(progn  (package-refresh-contents) ; package list not yet refreshed!
							(setq package-list-refreshed t)
							(package-install package))
			)
		)
	)
;; upgrade packages (this slows down start-up somewhat)
;(paradox-upgrade-packages)

;; use-package
(setq use-package-verbose t)

;;==========================================================
;;      MINIBUFFER SETUP 
;;==========================================================

(require 'setup-minibuffer)


;;==========================================================
;;      MAJOR MODES
;;==========================================================

;; (use-package multi-project
;; 	:ensure t
;; 	:config
;; 	(global-multi-project-mode))

(use-package web-mode										; for improved html support
	:ensure t
	:mode
	("\\.phtml\\'" . web-mode)
	("\\.tpl\\.php\\'" . web-mode)
	("\\.[agj]sp\\'" . web-mode)
	("\\.as[cp]x\\'" . web-mode)
	("\\.erb\\'" . web-mode)
	("\\.mustache\\'" . web-mode)
	("\\.djhtml\\'" . web-mode)
	("\\.html?\\'" . web-mode)
	("\\.xml\\'" . web-mode)
	("\\.css\\'" . web-mode)
)

(use-package js2-mode										; for improved JavaScript support
	:ensure t
	:mode
	("\\.js\\'" . js2-mode))

(use-package tex
	:ensure auctex												; because auctex overwrites tex
	:config
	)
(require 'setup-latex)									; TODO: include into (use-package tex)

(use-package markdown-mode
	:ensure t
	:config 
	(defun my-markdown-mode-config ()
		"settings for markdown mode"
		(interactive)
		(setq-default tab-width 4)
		(setq-default indent-tabs-mode t)
		(setq markdown-enable-math t))
	(add-hook 'markdown-mode 'my-markdown-mode-config)
	(setq markdown-enable-math t)
	)

(require 'setup-orgmode)


;;==========================================================
;;      BUFFER SETUP 
;;==========================================================

(require 'setup-buffer)


;;==========================================================
;;      MISCELLANEOUS
;;==========================================================

(require 'setup-speedbar)
(require 'setup-frame)

;; magit
(use-package magit
	:ensure t
	:bind
	("C-x g" . magit-status)
	;; ("C-x C-g" . magit-status)
	)

;; git-gutter
(use-package git-gutter
	:ensure t
	:config
	(global-git-gutter-mode +1)
	;; (git-gutter:linum-setup)
	(custom-set-variables
	 '(git-gutter:update-interval 2))
	:bind
	("C-x C-g" . nil)
	("C-x C-g TAB" . git-gutter:popup-hunk)
	("C-x C-g _" . git-gutter:revert-hunk)
	("C-x C-g C-g" . git-gutter-mode)
	)

;; adds support of the windows powershell
(if (eq system-type 'windows-nt)
		(use-package powershell
			:ensure t)
)
