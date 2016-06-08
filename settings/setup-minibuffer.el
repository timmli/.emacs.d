;; shorten yes/no answers to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; ido improves buffer switching experience
(ido-mode 1)
;; add flx to ido 
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


;; counsel adds fuzzy search to command completion 
(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(setq ivy-display-style 'fancy)
(setq ivy-re-builders-alist ; use flx
			'((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil) ; omit ^

;; ;; smex helps to remember often used commands; used by ido and counsel
(require 'smex)


;;==========================================================
;;      KEYS
;;==========================================================

;; M-x in minibuffer quits the minibuffer
(add-hook 'minibuffer-setup-hook
					(lambda ()
						(local-set-key (kbd "M-x") 'abort-recursive-edit)))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer) ; instead of 'list-buffers

(global-set-key (kbd "C-o") #'imenu-anywhere)
(global-set-key (kbd "C-S-o") #'imenu-list-minor-mode)
(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)

(provide 'setup-minibuffer)
