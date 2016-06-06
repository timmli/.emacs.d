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

;; cursor position history
(require 'point-undo)
(global-set-key [M-left] 'point-undo)
(global-set-key [M-right] 'point-redo)

(defun my-markdown-mode-config ()
	"settings for markdown mode"
	(interactive)
	(setq-default tab-width 4)
	(setq-default indent-tabs-mode t))
(add-hook 'markdown-mode 'my-markdown-mode-config)

;; adds support of the windows powershell
(require 'powershell)


(provide 'setup-buffer.el)
