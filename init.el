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

;; start emacs server
;; (server-start)  ; uncomment this, if you want the server to start on every start-up


;;==========================================================
;;      DIRECTORIES SETUP
;;==========================================================

;; set home directory
;; 1: check HOME variable
(when (or
			 (not (getenv "HOME"))
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
(defvar home-directory default-directory)

(when (getenv "EMACS_USER_DIRECTORY")
	(setq user-emacs-directory
				(expand-file-name (concat (getenv "EMACS_USER_DIRECTORY") "/"))))

;; set path to local lisp libaries
(defvar lisp-dir)
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path lisp-dir)

;; write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "temp")))))

;; save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;;==========================================================
;;      PRIVATE DIRECTORIES SETUP (outside .emacs.d)
;;==========================================================

;; set path to private org notes
(defvar org-directory
	(expand-file-name "org/" home-directory)) ; default value; may be overwritten by private directory settings

;; set path to the directory that deft observes
(defvar my-deft-dir org-directory) ; default value; may be overwritten by private directory settings

;; set path to authinfo
(defvar authinfo-directory home-directory) ; default value; may be overwritten by private directory settings

;; set path to private emacs settings
(defvar private-emacs-settings-dir
	(expand-file-name "Dropbox/emacs-settings/" home-directory)) ;; FIXME: make this more generic, e.g. with using links in .emacs.d
;; if system variable exists, use it
(when (getenv "PRIVATE_EMACS_SETTINGS")
	(setq private-emacs-settings-dir
				(expand-file-name (concat (getenv "PRIVATE_EMACS_SETTINGS") "/"))))

(require 'subr-x)  ; string-remove-prefix needs this
(defvar trimmed-private-emacs-settings-dir
	(concat "~/" (string-remove-prefix home-directory private-emacs-settings-dir))
	"This variable contains the trimmed version of the path in private-emacs-settings-dir starting with ~/.
  Using trimmed paths is necessary with some packages, e.g. calfw. 
  "	
	)

;; load private directory settings file
(let ((dir-settings-file (expand-file-name "directory-settings.el" private-emacs-settings-dir)))
	(when (file-exists-p dir-settings-file)
		(load-file dir-settings-file)))


;;==========================================================
;;      USER INFOS (outside .emacs.d)
;;==========================================================

;; GENERAL USER INFO
;; available variables 
(defvar user-acronym "user-acronym"
	"Contains the acronym of the user name. This is used, e.g. in cm-mode.")
;; load user info file
(let ((user-info-file (expand-file-name "user-info.el" private-emacs-settings-dir)))
	(when (file-exists-p user-info-file)
		(load-file user-info-file)))

;; MISCELLANEOUS USER SETTINGS
;; available variables
(defvar user-bibliography-file
	"/path/to/bibliography.bib"
	"Path to the user bibliography file.")
(defvar user-bibliography-pdf-dir
	"/path/to/pdfs/"
	"Path to the user directory of PDFs belonging to the bibliography.")
;; load file with miscellaneous private settings
(let ((misc-settings-file (expand-file-name "misc-settings.el" private-emacs-settings-dir)))
	(when (file-exists-p misc-settings-file)
		(load-file misc-settings-file)))

;;==========================================================
;;      WEMACS
;;=========================================================

(when (eq system-type 'windows-nt)

	(defvar wemacs-dir "") 
	(when (getenv "WEMACS_HOME")
		;; set path to wemacs folder with additional third-party software
		(setq wemacs-dir
					(expand-file-name (getenv "WEMACS_HOME"))))

	(when (getenv "WEMACS_PATH")
		;; using setenv makes available the PATH variable to the emacs shells
		(setenv "PATH" (expand-file-name (concat (getenv "WEMACS_PATH") ";" (getenv "PATH"))))
		;; using exec-path makes available the PATH variable to the rest of emacs
		(setq exec-path
					(append (split-string (expand-file-name (getenv "WEMACS_PATH")) ";") exec-path))
		;; (setq exec-path (concat (getenv "PATH") ";" exec-path))  
		)

	;; the following can be left implicit
	;; ;; use ported gnu find command under windows
	;; ;; findutils seems to be faster than gnuwin32
	;; (setq find-program
	;; 			(expand-file-name
	;; 			 (concat wemacs-dir "/findutils/bin/find")))

	;; ;; use external ls instead of ls-lisp
	;; ;; (caveat: grouping of directories might not work out of the box any more.)
	;; (setq ls-lisp-use-insert-directory-program t)
	
	;; make PC keyboard's Win key to type Super or Hyper, for emacs running on Windows.
	(progn
		(setq w32-pass-lwindow-to-system nil)
		(setq w32-lwindow-modifier 'super) ; Left Windows key
		)	

	)

;;==========================================================
;;      CUSTOM.EL
;;=========================================================

;; font
(defvar custom-default-font "DejaVu Sans Mono" nil)
(defvar custom-fixed-pitch-font "DejaVu Sans Mono" nil)
(defvar custom-variable-pitch-font "Segoe UI" nil) 
(when (not (find-font (font-spec :name "Segoe UI"))) ; Segoe UI might be unavailable on Linux
	(setq custom-variable-pitch-font "Roboto"))

;; ;; default (can be overridden by custom.el)
;; (add-to-list 'default-frame-alist
;;              '(font . "DejaVu Sans Mono-11"))
;; stolen from custom.el (can be overridden by custom.el)
(custom-set-faces
 `(default ((t (:family ,custom-default-font :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 `(fixed-pitch ((t (:family ,custom-fixed-pitch-font :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 `(variable-pitch ((t (:family ,custom-variable-pitch-font :foundry "outline" :slant normal :weight normal :height 125 :width normal))))
 )

;; load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file)
  (message "Warning: custom.el not found.")
)

;;==========================================================
;;      PACKAGE MANAGEMENT
;;==========================================================

(when (>= emacs-major-version 24)
  (require 'package)
	(setq package-archives
				'(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
					("MELPA Stable" . "https://stable.melpa.org/packages/")
					("MELPA"        . "https://melpa.org/packages/")
					("org" . "https://orgmode.org/elpa/"))
				package-archive-priorities
				'(("MELPA Stable" . 10)
					("GNU ELPA"     . 5)
					("MELPA"        . 0)
					("org"          . 0)))
	;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
	;; (add-to-list 'package-archives 				; repository of the sunrise commander
	;; 						 '("SC"   . "http://joseito.republika.pl/sunrise-commander/") t)
  (package-initialize))
;; list the packages you want
(setq package-list '(async 							; paradox needs async
										 paradox
										 use-package
										 org
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

;; upgrade org-mode if necessary
(require 'org)
(when (version< org-version "9")
	(when (not package-list-refreshed) 				; package list already refreshed?
		(package-refresh-contents) ; package list not yet refreshed!
		(setq package-list-refreshed t))
	(paradox-upgrade-packages)
	(require 'org))

;; use-package
;; taken from https://github.com/jwiegley/use-package
(eval-when-compile											
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)               
(setq use-package-verbose t)


;;==========================================================
;;      LOAD REMAINING SETTINGS
;;==========================================================

;; the remaining settings are in org-init.org
(require 'org)
(defvar org-init-file "org-init.org")
(org-babel-load-file
 (expand-file-name org-init-file
                   user-emacs-directory))

(put 'dired-find-alternate-file 'disabled nil)
