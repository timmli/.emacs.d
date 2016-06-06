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


(provide 'setup-minibuffer)
