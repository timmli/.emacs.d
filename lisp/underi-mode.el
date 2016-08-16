;;; underi-mode.el --- Keymap for the 21th century 

;; Copyright (C) 2016 Timm Lichte

;; Author: Timm Lichte <lichte@phil.hhu.de>
;; Version: 0.1

;;; Commentary:

;; TODO

;;; Code:


(require 'helm)

(define-minor-mode underi-mode
	"Keymap for the 21th century.
Inspired by: http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html"
	:lighter " ui"
	;; :global "t"
	:keymap (let ((map (make-keymap)))

						(define-key input-decode-map (kbd "C-i") (kbd "H-i")) ; to disentangle <tab> and C-i
						(define-key key-translation-map (kbd "M-i") (kbd "<up>"))
						(define-key key-translation-map (kbd "M-k") (kbd "<down>"))
						(define-key key-translation-map (kbd "M-j") (kbd "<left>"))		
						(define-key key-translation-map (kbd "M-l") (kbd "<right>"))
						(define-key key-translation-map (kbd "M-I") (kbd "S-<up>"))
						(define-key key-translation-map (kbd "M-K") (kbd "S-<down>"))
						(define-key key-translation-map (kbd "M-J") (kbd "S-<left>"))		
						(define-key key-translation-map (kbd "M-L") (kbd "S-<right>"))
	
						;; move cursor
						(define-key map (kbd "M-n") 'scroll-up-command)
						(define-key map (kbd "M-p") 'scroll-down-command)
						(define-key map (kbd "M-o") 'point-redo)
						(define-key map (kbd "M-u") 'point-undo)
						(define-key map (kbd "M-z") 'goto-last-change)
						(define-key map (kbd "C-M-i") 'backward-paragraph)
						(define-key map (kbd "C-M-k") 'forward-paragraph)
						(define-key map (kbd "C-M-j") 'left-word)
						(define-key map (kbd "C-M-l") 'right-word)

						(define-key map (kbd "M-s-j")  'windmove-left)
						(define-key map (kbd "M-s-l") 'windmove-right)
						(define-key map (kbd "M-s-i")   'windmove-up)
						(define-key map (kbd "M-s-k")  'windmove-down)

						(define-key key-translation-map (kbd "C-n") (kbd "C-c C-n"))
						(define-key key-translation-map (kbd "C-p") (kbd "C-c C-p"))

						;; delete 
						(define-key map (kbd "C-d") nil)
						(define-key map (kbd "C-d C-k") 'kill-line)
						(define-key map (kbd "C-d H-i") '(lambda () (interactive) (kill-line 0)))
						(define-key map (kbd "C-d C-j") 'delete-backward-char)
						(define-key map (kbd "C-d C-l") 'delete-forward-char)
						(define-key map (kbd "C-d C-e") 'kill-line)
						(define-key map (kbd "C-d C-o") 'delete-blank-lines)
						(define-key map (kbd "C-d C-m") 'delete-blank-lines)
						(define-key map (kbd "C-d C-a") '(lambda () (interactive) (kill-line 0)))
						(define-key map (kbd "C-S-d") 'kill-whole-line)

						;; miscellaneous actions
						;; (define-key map (kbd "C-s") 'save-buffer)
						;; (define-key map (kbd "C-S-s") 'save-some-buffers)
						;; (define-key map (kbd "C-x C-s") 'helm-occur)
						(define-key map (kbd "C-f") 'isearch-search)
						(define-key map (kbd "C-x C-a") 'mark-whole-buffer)
						;; (define-key map (kbd "C-p") 'recenter-top-bottom)
						(define-key key-translation-map (kbd "M-q") (kbd "C-g"))
						(define-key map (kbd "C-z") 'undo-tree-undo)
						(define-key map (kbd "C-S-z") 'undo-tree-redo)
						(define-key map (kbd "C-j") 'ace-jump-mode)
						(define-key map (kbd "H-i") 'helm-imenu)
						(define-key map (kbd "C-S-i") 'imenu-list)
						
						map
						)
	(add-hook 'minibuffer-setup-hook 'underi-minibuffer)
	
)	
;; TODO: make C-d switch to delete-mode?
;; http://stackoverflow.com/a/12010437/6452961

(defun underi-minibuffer ()
	""
	(let ((map minibuffer-local-map))

		;; move cursor
		(define-key map (kbd "M-n") 'scroll-up-command)
		(define-key map (kbd "M-p") 'scroll-down-command)
		(define-key map (kbd "M-o") 'point-redo)
		(define-key map (kbd "M-u") 'point-undo)
		(define-key map (kbd "M-z") 'goto-last-change)
		(define-key map (kbd "C-M-i") 'backward-paragraph)
		(define-key map (kbd "C-M-k") 'forward-paragraph)
		(define-key map (kbd "C-M-j") 'left-word)
		(define-key map (kbd "C-M-l") 'right-word)
		
		;; delete 
		(define-key map (kbd "C-d") nil)
		(define-key map (kbd "C-d C-k") 'kill-line)
		(define-key map (kbd "C-d C-i") '(lambda () (interactive) (kill-line 0)))
		(define-key map (kbd "C-d C-j") 'delete-backward-char)
		(define-key map (kbd "C-d C-l") 'delete-forward-char)
		(define-key map (kbd "C-d C-e") 'kill-line)
		(define-key map (kbd "C-d C-a") '(lambda () (interactive) (kill-line 0)))
		(define-key map (kbd "C-S-d") 'kill-whole-line)

		))

(add-hook 'text-mode-hook 'underi-mode)
(add-hook 'prog-mode-hook 'underi-mode)

(provide 'underi-mode)

;;; underi-mode.el ends here

