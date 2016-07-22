;; shorten yes/no answers to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; ido improves buffer switching experience
(ido-mode 1)
(ido-everywhere 1)
;; add vertical mode to ido
(use-package ido-vertical-mode	
	:ensure t
	:config (ido-vertical-mode 1)	)
;; add grid mode
(use-package ido-grid-mode
	:ensure t
	:config (ido-grid-mode 1))												
;; add flx to ido 
(use-package flx-ido
	:ensure t
	:config 
	(flx-ido-mode 1)
	;; disable ido faces to see flx highlights.
	(setq ido-enable-flex-matching t)
	(setq ido-use-faces nil))


;; recent files
(require 'recentf)
(recentf-mode 1)
; 50 files ought to be enough.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
	"Use `ido-completing-read' to \\[find-file] a recent file"
	(interactive)
	(if (find-file (ido-completing-read "Find recent file: " recentf-list))
			(message "Opening file...")
		(message "Aborting")))


;; ivy componente
;; counsel adds fuzzy search to command completion 
(use-package counsel
	:ensure t
	:config
	(setq ivy-display-style 'fancy)
	(setq ivy-re-builders-alist ; use flx
				'((t . ivy--regex-fuzzy)))
	(setq ivy-initial-inputs-alist nil) ; omit ^
	(setq ivy-wrap t) ;; cycle through results
	:bind
	("M-x" . counsel-M-x)
	("C-ß" . ivy-imenu-anywhere) ; ivy + imenu
	)
;; swiper
(use-package swiper
	:ensure t
	:config
	;; use swiper for buffer search
	(add-hook 'prog-mode-hook
						(lambda ()
							(local-set-key (kbd "C-s") 'swiper)))
	;; (global-set-key (kbd "C-s") 'swiper) ; not good in text-mode
	)

;; imenu
(use-package imenu-anywhere
	:ensure t)
(use-package imenu-list
	:ensure t
	:bind
	("C-?" . imenu-list-minor-mode)
	:config
	(setq imenu-list-focus-after-activation t)
	(setq imenu-list-auto-resize t)
	)

;; smex helps to remember often used commands; used by ido and counsel
(use-package smex
	:ensure t)


;;==========================================================
;;      KEYS
;;==========================================================

;; M-x in minibuffer quits the minibuffer
(add-hook 'minibuffer-setup-hook
					(lambda ()
						(local-set-key (kbd "M-x") 'abort-recursive-edit)))

;; C-ß in minibuffer quits the minibuffer
(add-hook 'minibuffer-setup-hook
					(lambda ()
						(local-set-key (kbd "C-ß") 'abort-recursive-edit)))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer) ; instead of 'list-buffers
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)  




(provide 'setup-minibuffer)
