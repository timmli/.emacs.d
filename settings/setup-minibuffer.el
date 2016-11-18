;; shorten yes/no answers to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;==========================================================
;;      IDO, IVY, IMENU
;;==========================================================

;; ;; ido improves buffer switching experience
;; (ido-mode 1)
;; (ido-everywhere 1)
;; ;; add vertical mode to ido
;; (use-package ido-vertical-mode	
;; 	:ensure t
;; 	:config (ido-vertical-mode 1)	)
;; ;; add grid mode
;; (use-package ido-grid-mode
;; 	:ensure t
;; 	:config (ido-grid-mode 1))												
;; ;; add flx to ido 
;; (use-package flx-ido
;; 	:ensure t
;; 	:config 
;; 	(flx-ido-mode 1)
;; 	;; disable ido faces to see flx highlights.
;; 	(setq ido-enable-flex-matching t)
;; 	(setq ido-use-faces nil))


;; ;; recent files
;; (require 'recentf)
;; (recentf-mode 1)
;; ; 50 files ought to be enough.
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; (setq recentf-max-saved-items 50)
;; (defun ido-recentf-open ()
;; 	"Use `ido-completing-read' to \\[find-file] a recent file"
;; 	(interactive)
;; 	(if (find-file (ido-completing-read "Find recent file: " recentf-list))
;; 			(message "Opening file...")
;; 		(message "Aborting")))


;; ;; ivy componente
;; ;; counsel adds fuzzy search to command completion 
;; (use-package counsel
;; 	:ensure t
;; 	:config
;; 	(setq ivy-display-style 'fancy)
;; 	(setq ivy-re-builders-alist ; use flx
;; 				'((t . ivy--regex-fuzzy)))
;; 	(setq ivy-initial-inputs-alist nil) ; omit ^
;; 	(setq ivy-wrap t) ;; cycle through results
;; 	:bind
;; 	("M-x" . counsel-M-x)
;; 	("C-ß" . ivy-imenu-anywhere) ; ivy + imenu
;; 	)
;; ;; swiper
;; (use-package swiper
;; 	:ensure t
;; 	:config
;; 	;; use swiper for buffer search
;; 	(add-hook 'prog-mode-hook
;; 						(lambda ()
;; 							(local-set-key (kbd "C-s") 'swiper)))
;; 	;; (global-set-key (kbd "C-s") 'swiper) ; not good in text-mode
;; 	)

;; imenu
(use-package imenu-anywhere
	:ensure t)
(use-package imenu-list
	:ensure t
	:bind
	("C-?" . imenu-list)
	:init
	(setq imenu-list-focus-after-activation t)
	;; (setq imenu-list-auto-resize t)
	(setq imenu-list-position (quote left))
	(setq imenu-list-size 30)
	:config
	(add-hook 'text-mode-hook 'imenu-list-minor-mode)
	(add-hook 'prog-mode-hook 'imenu-list-minor-mode)
	)
(add-hook 'imenu-list-minor-mode-hook (lambda () (toggle-truncate-lines))) ; FIXME
(setq org-imenu-depth 4)

;; ;; smex helps to remember often used commands; used by ido and counsel
;; (use-package smex
;; 	:ensure t)

;;==========================================================
;;      HELM
;;==========================================================

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01		; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t		 ; do not display invisible candidates
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
					helm-mode-fuzzy-match t 			; global fuzzy match
					helm-buffers-fuzzy-matching t
					helm-recentf-fuzzy-match t
					helm-M-x-fuzzy-match t
					helm-imenu-fuzzy-match t
					helm-completion-in-region-fuzzy-match t
					helm-apropos-fuzzy-match t
					helm-autoresize-mode 1 				; re-size the completion window based on number of candidates
					helm-adaptive-mode t					; show commonly used commands first
					)
		(setq bibtex-completion-bibliography (concat home-directory "/Dropbox/Forschung/timm-bib.bib")
					bibtex-completion-library-path (concat home-directory "/ownCloud/Bib") ; directory of PDFs
					;; bibtex-completion-notes-path "~/Dropbox/bibliography/helm-bibtex-notes"
					)
    (helm-mode)

		;; http://emacs.stackexchange.com/a/7896/12336
		;; <return> opens directory in helm-find-files, not dired
		(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
			(if (file-directory-p (helm-get-selection))
					(apply orig-fun args)
				(helm-maybe-exit-minibuffer)))
		(advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
		(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
		;; <backspace> before backslash lets helm-find-files  move one directory up
		(defun fu/helm-find-files-navigate-back (orig-fun &rest args)
			(if (= (length helm-pattern) (length (helm-find-files-initial-input)))
					(helm-find-files-up-one-level 1)
				(apply orig-fun args)))
		(advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)
		;; https://redd.it/3f55nm
		;; remove . and .. from helm-find-files
		(advice-add 'helm-ff-filter-candidate-one-by-one
								:around (lambda (fcn file)
													(unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
														(funcall fcn file))))
		)
  :bind (("M-y" . helm-mini)
				 ("C-x C-r" . helm-recentf)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
				 ("C-x C-f" . helm-find-files)
         ("C-x C-y" . helm-show-kill-ring)
         ("C-x y" . helm-show-kill-ring)
         ("C-x SPC" . helm-all-mark-rings)
         ("C-x C-SPC" . helm-all-mark-rings)				 
         ("M-x" . helm-M-x)
         ("C-s" . helm-occur)
         ;; ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)
				 ("C-ß" . helm-imenu)
				 ("C-S-?" . helm-imenu-anywhere)
				 )
	:config
	(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; complete with <tab> (default is <ret>)
	(define-key helm-map (kbd "C-z") 'helm-select-action) ; show actions (default is <tab>)
)
(ido-mode -1)														; turn off ido mode, just in case

;; helm-flx: improves fuzzy matching
(use-package helm-flx
	:ensure t
	:after helm
	:config
	(helm-flx-mode +1))
;; helm-fuzzier: improves fuzzy matching even more by taking more candidates into account
(use-package helm-fuzzier
	:ensure t
	:after helm
	:config
	(helm-fuzzier-mode +1))

;; list active key bindings 
(use-package helm-descbinds
	:ensure t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-swoop
	:ensure t
	:config
	;; Move up and down like isearch
	(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
	(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
	(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
	(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

	;; From helm-swoop to helm-multi-swoop-all
	(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

	;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
	(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
	
	;; If nil, you can slightly boost invoke speed in exchange for text color
	(setq helm-swoop-speed-or-color t)
	
	;; Optional face for line numbers
	;; Face name is `helm-swoop-line-number-face`
	(setq helm-swoop-use-line-number-face t)

	;; If you prefer fuzzy matching (seems to be already activated)
	;; (setq helm-swoop-use-fuzzy-match t)

	;; Do not call helm-swoop with symbol or word at point
	(setq helm-swoop-pre-input-function
				(lambda () nil))

  :bind ("C-c /" . helm-swoop))


;;==========================================================
;;      KEYS
;;==========================================================

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

;; M-x in minibuffer quits the minibuffer
(add-hook 'minibuffer-setup-hook
					(lambda ()
						(local-set-key (kbd "M-x") 'abort-recursive-edit)))

;; M-y in minibuffer quits the minibuffer
(add-hook 'minibuffer-setup-hook
					(lambda ()
						(local-set-key (kbd "M-y") 'abort-recursive-edit)))

;; C-ß in minibuffer quits the minibuffer
(add-hook 'minibuffer-setup-hook
					(lambda ()
						(local-set-key (kbd "C-ß") 'abort-recursive-edit)))

;; C-s in minibuffer quits the minibuffer
(add-hook 'minibuffer-setup-hook
					(lambda ()
						(local-set-key (kbd "C-s") 'abort-recursive-edit)))

;; (global-set-key (kbd "C-x C-b") 'switch-to-buffer) ; instead of 'list-buffers (see helm)
;; (global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)  




(provide 'setup-minibuffer)
