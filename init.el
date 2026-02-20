;;==========================================================
;;      GENERAL SETUP
;;==========================================================

;; A tip from https://emacsredux.com/blog/2025/03/28/speed-up-emacs-startup-by-tweaking-the-gc-settings/
;; Temporarily increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
;; Restore to normal value after startup (e.g. 50MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

;; Don't show startup message
(setq inhibit-startup-message t)

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE 
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))

;; Start emacs server
;; (server-start)  ; uncomment this, if you want the server to start on every start-up

;; Emacs 28: Use the older, more tolerant version of seq-empty-p.
(when (= emacs-major-version 28)
	(require 'seq-24))

;; Disable Tramp to prevent mounting archive files 
(setq tramp-mode nil)


;;==========================================================
;;      DIRECTORIES SETUP
;;==========================================================

;; Set home directory
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

;; Set path to local lisp libaries
(defvar lisp-dir)
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path lisp-dir)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;;================================================================
;;      PRIVATE SETTINGS: DIRECTORIES & FILES  (outside .emacs.d)
;;================================================================

(defvar org-directory
	(expand-file-name "org/" home-directory) ; default value; may be overwritten by private directory settings
	"Path to private org notes.
This variable should be changed in private-emacs-settings-before.el.") 

(defvar my-deft-dir org-directory ; default value; may be overwritten by private directory settings
	"Path to the directory that deft observes.
This variable should be changed in private-emacs-settings-before.el.")

(defvar my-org-roam-directory org-directory ; default value; may be overwritten by private directory settings
	"Path to the directory that org-roam uses.
This variable should be changed in private-emacs-settings-before.el.")

(defvar authinfo-directory home-directory ; default value; may be overwritten by private directory settings
	"Path to authinfo.
This variable should be changed in private-emacs-settings-before.el.")

(defvar private-emacs-settings-dir
	(expand-file-name "private-emacs-settings/" user-emacs-directory) ; default value; may be overwritten by system variable PRIVATE_EMACS_SETTINGS
	"Path to private Emacs settings directory.
This variable can be changed with the system variable PRIVATE_EMACS_SETTINGS.")

(if (getenv "PRIVATE_EMACS_SETTINGS")
		(message "Info: PRIVATE_EMACS_SETTINGS=%s" (expand-file-name (getenv "PRIVATE_EMACS_SETTINGS")))
	(message "Warning: system variabel PRIVATE_EMACS_SETTINGS"))

(defvar my-bbdb-file
	(expand-file-name "bbdb" user-emacs-directory)
	"Path to bbdb file.
This variable should be changed in private-emacs-settings-before.el.")

(require 'bookmark)
(defvar my-bookmarks-file
	(concat bookmark-default-file)
	"Path to bookmarks file.
This variable should be changed in private-emacs-settings-before.el.")

;; If system variable exists, use it
(when (getenv "PRIVATE_EMACS_SETTINGS")
	(setq private-emacs-settings-dir
				(expand-file-name (concat (getenv "PRIVATE_EMACS_SETTINGS") "/"))))

(require 'subr-x)  ; string-remove-prefix needs this
(defvar trimmed-private-emacs-settings-dir
	(concat "~/" (string-remove-prefix home-directory private-emacs-settings-dir))
	"This variable contains the trimmed version of the path in private-emacs-settings-dir starting with ~/.
Using trimmed paths is necessary with some packages, e.g. calfw. 
  ")

(defvar docsets-dir
	(expand-file-name "docsets/" home-directory)
	"Path to docsets directory.
This variable should be changed in private-emacs-settings-before.el.")

(defvar music-dir
	(expand-file-name "Music/" home-directory)
	"Path to music directory.
This variable should be changed in private-emacs-settings-before.el.")

(defvar backup-dir
	(expand-file-name "backup/" user-emacs-directory)
	"Path to backup directory.
This variable should be changed in private-emacs-settings-before.el.")

(defvar mail-dir
	(expand-file-name "Maildir/" home-directory)
	"Path to mail directory.
This variable should be changed in private-emacs-settings-before.el.")

(defvar personal-dictionary-file
	nil
	"Path to personal dictionary file that is used by ispell.
This variable should be changed in private-emacs-settings-before.el.")

(defvar personal-dictionary-dir
	nil
	"Path to directory with personal dictionaries that is used by ispell.
This variable should be changed in private-emacs-settings-before.el.")

(defvar langtool-dir
	nil
	"Path to directory with langtool resources.
This variable should be changed in private-emacs-settings-before.el.")

(defvar share-dir
	(expand-file-name "share/" user-emacs-directory)
	"Path to directory with shared resources.
This variable should be changed in private-emacs-settings-before.el.")

;;==========================================================
;;      PRIVATE SETTINGS: USER INFO (outside .emacs.d)
;;==========================================================

;; GENERAL USER INFO
(defvar user-acronym "user-acronym"
	"Contains the acronym of the user name, which is used, e.g., in cm-mode.
This variable should be changed in private-emacs-settings.el.")

;; MISCELLANEOUS USER SETTINGS
(defvar user-bibliography-file
	"/path/to/bibliography.bib"
	"Path to the user bibliography file.
This variable should be changed in private-emacs-settings.el.")

(defvar user-bibliography-pdf-dir
	"/path/to/pdfs/"
	"Path to the user directory of PDFs belonging to the bibliography.
This variable should be changed in private-emacs-settings.el.")

(defvar user-bibliography-notes-dir
	"/path/to/pdfs/"
	"Path to the user directory of notes belonging to the bibliography.
This variable should be changed in private-emacs-settings.el.")

(defvar default-csl-style-file
	(expand-file-name "csl/minimal-unified-style-sheet-for-computational-linguistics.csl" user-emacs-directory)
	"Path to the default CSL style file.")

(defvar default-csl-locale-dir 
	(expand-file-name "csl/" user-emacs-directory)
	"Path to the default CSL locale dir.")

(defvar user-deepl-key
	"key"
	"Authentication Key for DeepL API
This variable should be changed in private-emacs-settings.el.")


;; load file with private settings before generic settings
(let ((private-settings-file (expand-file-name "private-emacs-settings-before.el" private-emacs-settings-dir)))
	(when (file-exists-p private-settings-file)
		(load-file private-settings-file)))


;;==========================================================
;;      CUSTOM.EL
;;=========================================================

;; Font
(defvar custom-default-font "DejaVu Sans Mono" nil)
(defvar custom-fixed-pitch-font "DejaVu Sans Mono" nil)
(defvar custom-variable-pitch-font "Segoe UI" nil)

;; ;; The following was supposed to check the availability of Segoe UI with a fall-back option.
;; ;; Unfortunately, `find-font` only works as expected when a frame
;; ;; is already present, which is not the case when loading the emacs daemon. 
;; ;; A solution is described here: https://emacs.stackexchange.com/questions/12351/when-to-call-find-font-if-launching-emacs-in-daemon-mode
;; (when (not (find-font (font-spec :name "Segoe UI"))) ; Segoe UI might be unavailable on Linux
;; (setq custom-variable-pitch-font "Arial"))

;; Stolen from custom.el (can be overridden by custom.el)
(custom-set-faces
 `(default ((t (:family ,custom-default-font :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 `(fixed-pitch ((t (:family ,custom-fixed-pitch-font :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 `(variable-pitch ((t (:family ,custom-variable-pitch-font :foundry "outline" :slant normal :weight normal :height 125 :width normal))))
 )

;; Load custom file
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
;; List the packages you want
(setq package-list '(async 							; paradox needs async
										 paradox
										 use-package				; built-in as of v29 
										 org								; built-in
                     ))
;; ;; Fetch the list of packages available
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; Install the missing packages (and refresh the package list if necessary)
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
;; Upgrade packages (this slows down start-up somewhat)
;; (paradox-upgrade-packages)

;; Upgrade org-mode if necessary
;; (require 'org)
;; (when (version< org-version "9.3")
;; 	(when (not package-list-refreshed) 				; package list already refreshed?
;; 		(package-refresh-contents) ; package list not yet refreshed!
;; 		(setq package-list-refreshed t))
;; 	(paradox-upgrade-packages)
;; 	(require 'org))

;; use-package
;; Taken from https://github.com/jwiegley/use-package
(eval-when-compile											
  (require 'use-package))
(setq use-package-verbose t)
(use-package diminish										; if you use :diminish
	:ensure t)
(require 'bind-key)

;; Quelpa: a tool to compile and install Emacs Lisp packages
(use-package quelpa
	:ensure t)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; straight.el: yet another (purely functional) package manager
;; https://github.com/radian-software/straight.el
;; Straight emits a warning if package.el is present and
;; loaded. Silence it.
(add-to-list 'warning-suppress-types '(straight package))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ;; el-get
;; ;; https://github.com/dimitri/el-get
;; (use-package el-get
;; 	:pin MELPA
;; 	:ensure t)
;; ;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;; ;; (setq el-get-recipe-path '("~/.emacs.d/el-get/el-get/recipes"))
;; (el-get 'sync)

;;==========================================================
;;      LOAD REMAINING SETTINGS
;;==========================================================

;; The remaining settings are in org-init.org
(setq org-fold-core-style 'overlays) 		; Org v9.6 comes with a new folding technique.
																				; Use the old folding technique with overlays for
																				; better interoperability with 3rd-party packages.
																				; This variable must be set before loading Org.
(require 'org)
(defvar org-init-file "org-init.org")
(org-babel-load-file
 (expand-file-name org-init-file
                   user-emacs-directory))

(put 'dired-find-alternate-file 'disabled nil)
