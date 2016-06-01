
;; package managing
(when (>= emacs-major-version 24)
  (require 'package)
  ;; (add-to-list 'package-archives
	;;        '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))
;; list the packages you want
(setq package-list '(auctex auto-complete auto-complete-auctex counsel flycheck flx-ido ido-grid-mode ido-vertical-mode smartparens smex monokai-theme sr-speedbar ))
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
;; add vertical mode to ido
(ido-vertical-mode 1)
;; add grid mode
(ido-grid-mode 1)

;; counsel adds fuzzy search to command completion 
(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(setq ivy-display-style 'fancy)
(setq ivy-re-builders-alist ; use flx
      '((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil) ; omit ^

;; ;; smex helps to remember often used commands; used by ido and counsel
(require 'smex)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-auctex)
(global-auto-complete-mode 1)
;; (ac-config-default)
;; (add-to-list 'ac-modes 'latex-mode)     ; activate auto-complete for latex <modes (AUCTeX or Emacs' builtin one).

;; flycheck
(require 'flycheck)
(global-flycheck-mode t)

;; smartparens
(require 'smartparens)
(smartparens-mode t)

;; LaTeX settings
(if (file-exists-p "~/.emacs.d/mySettings/extas-latex.el")
    (load-file "~/.emacs.d/mySettings/extras-latex.el"))

;; cursor position history
(load-file "~/.emacs.d/mySettings/point-undo.el")
(global-set-key [M-left] 'point-undo)
(global-set-key [M-right] 'point-redo)

;; sr-speedbar
(require 'sr-speedbar)
(global-set-key (kbd "C-x C-k C-b") 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)                          ; always on left side
(add-hook 'speedbar-mode-hook '(lambda () (linum-mode 0))) ; disable linum for speedbar
(setq speedbar-show-unknown-files t)                       ; show all files
(setq sr-speedbar-width 30)                                ; default width


;;; extras.el ends here
