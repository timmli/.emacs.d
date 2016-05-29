
;; package managing
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
	;;        '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))
;; list the packages you want
(setq package-list '(auctex auto-complete counsel flycheck flx-ido smartparens monokai-theme ))
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; theme
(load-theme 'monokai t)

;; add flx to ido (already loaded in basic.el)
(require 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; counsel adds fuzzy search to command completion 
(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(setq ivy-display-style 'fancy)
(setq ivy-re-builders-alist ; use flx
      '((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil) ; omit ^

;; ;; smex helps to remember often used commands; used by ido and counsel
;; (require 'smex)

;; auto-complete
(require 'auto-complete)
(auto-complete-mode 1)
(ac-config-default)

;; flycheck
(require 'flycheck)
(global-flycheck-mode t)

;; smartparens
(require 'smartparens)
(smartparens-mode t)

;; LaTeX settings
(if (file-exists-p "~/.emacs.d/mySettings/extas-latex.el")
    (load-file "~/.emacs.d/mySettings/extras-latex.el"))

;; history
(load-file "~/.emacs.d/mySettings/point-undo.el")
(global-set-key [M-left] 'point-undo)
(global-set-key [M-right] 'point-redo)

