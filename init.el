;;==========================================================
;;      GENERAL SETUP
;;==========================================================

;; speed up start
(setq gc-cons-threshold 100000000)			; provide more memory

;; don't show startup message
(setq inhibit-startup-message t)

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


;;==========================================================
;;      DIRECTORIES SETUP
;;==========================================================

;; set home directory
;; 1: check HOME variable
(when (or
			 (string-equal (getenv "HOME") nil)
			 (string-equal (getenv "HOME") "")
			 )
	(if (y-or-n-p  "Environment variable HOME not set! Do this now?")
			(progn
				(call-interactively 'tl-ask-home)
				)
		(progn )
		)
	)
;; 2: ask for HOME path
(defun tl-ask-home (path)
	(interactive "sHOME: ")
	(setenv "HOME" path)
	(message "HOME variable was set to: %s (only for this session)" path)
	)
;; 3: set default-directory
(setq default-directory (expand-file-name (concat (getenv "HOME") "/")))
(defvar home-directory)
(setq home-directory default-directory)

;; set path to settings
(defvar settings-dir)
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)

;; set path to local lisp libaries
(defvar lisp-dir)
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path lisp-dir)

;; set path to wemacs folder with additional third-party software
(defvar wemacs-dir)
(setq wemacs-dir
      (expand-file-name "ownCloud/wemacs" home-directory))


;; set path to notes directory
(defvar notes-dir)
(setq notes-dir
      (expand-file-name "Dropbox/Notizen" default-directory))

;; write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "temp")))))

;; save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(if (eq system-type 'windows-nt)
		;; make PC keyboard's Win key to type Super or Hyper, for emacs running on Windows.
		(progn
			(setq w32-pass-lwindow-to-system nil)
			(setq w32-lwindow-modifier 'super) ; Left Windows key
			))

;;==========================================================
;;      CUSTOM.EL
;;=========================================================

;; font
;; ;; default (can be overridden by custom.el)
;; (add-to-list 'default-frame-alist
;;              '(font . "DejaVu Sans Mono-11"))
;; stolen from custom.el (can be overridden by custom.el)
(custom-set-faces
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 '(fixed-pitch ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 '(variable-pitch ((t (:family "Segoe UI" :foundry "outline" :slant normal :weight normal :height 125 :width normal))))
 )
;; load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file)
  (message "Warning: custom.el not found.")
)

;; use ported gnu find command under windows
;; findutils seems to be faster than gnuwin32
(when (eq system-type 'windows-nt)
	(setq find-program
				(expand-file-name
				 (concat home-directory "/ownCloud/wemacs/findutils/bin/find"))))

;;==========================================================
;;      PACKAGE MANAGEMENT
;;==========================================================

(when (>= emacs-major-version 24)
  (require 'package)
  ;; (add-to-list 'package-archives
	;;        '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
	;; 						 '("melpa" . "http://elpa.zilongshanren.com/melpa/") t)

	;; (add-to-list 'package-archives 				; repository of the sunrise commander
	;; 						 '("SC"   . "http://joseito.republika.pl/sunrise-commander/") t)
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
;; taken from https://github.com/jwiegley/use-package
(eval-when-compile											
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)               
(setq use-package-verbose t)


;; the remaining settings are in org-init.org
(require 'org)
(defvar org-init-file "org-init.org")
(org-babel-load-file
 (expand-file-name org-init-file
                   user-emacs-directory))

(put 'dired-find-alternate-file 'disabled nil)
