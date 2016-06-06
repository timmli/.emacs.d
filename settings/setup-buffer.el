;; automatically update buffers when files change
(global-auto-revert-mode t)

;; delete marked text on typing
(delete-selection-mode t)

;; don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode t)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-auctex)
(global-auto-complete-mode 1)
;; (ac-config-default)
;; (add-to-list 'ac-modes 'latex-mode)     ; activate auto-complete for latex <modes (AUCTeX or Emacs' builtin one).

;; flycheck
(require 'flycheck)
(global-flycheck-mode t)

;; flyspell
(setq ispell-program-name "")

;; smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode
          scala-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; expand-region (intelligent selction)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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


;;==========================================================
;;      KEYS
;;==========================================================

;; commenting
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))


(provide 'setup-buffer)
