;;; underi-mode.el --- Keymap for the 21th century -*- lexical-binding: t -*-

;; Copyright (C) 2016 Timm Lichte

;; Author: Timm Lichte <lichte@phil.hhu.de>
;; Version: 0.1

;;; Commentary:

;; TODO

;;; Code:


(require 'helm)

(define-minor-mode underi-mode
	"Keymap for the 21th century."
	:lighter " ui"
	;; :global "t"
	:keymap (let ((map (make-keymap)))

						(define-key input-decode-map (kbd "C-i") (kbd "H-i")) ; to disentangle <tab> and C-i
						(define-key key-translation-map (kbd "H-i") (kbd "<up>"))
						(define-key key-translation-map (kbd "C-k") (kbd "<down>"))
						(define-key key-translation-map (kbd "C-j") (kbd "<left>"))		
						(define-key key-translation-map (kbd "C-l") (kbd "<right>"))
						(define-key key-translation-map (kbd "H-S-i") (kbd "S-<up>"))
						(define-key key-translation-map (kbd "C-S-k") (kbd "S-<down>"))
						(define-key key-translation-map (kbd "C-S-j") (kbd "S-<left>"))		
						(define-key key-translation-map (kbd "C-S-l") (kbd "S-<right>"))
						;; (define-key map (kbd "H-i") 'previous-line)
						;; (define-key map (kbd "C-k") 'next-line)
						;; (define-key map (kbd "C-j") 'left-char)
						;; (define-key map (kbd "C-l") 'right-char)

						;; move cursor
						(define-key map (kbd "M-S-i") 'scroll-up-command)
						(define-key map (kbd "M-S-i") 'scroll-down-command)
						(define-key map (kbd "C-o") 'point-redo)
						(define-key map (kbd "C-u") 'point-undo)
						(define-key map (kbd "M-u") 'goto-last-change)
						(define-key map (kbd "M-i") 'backward-paragraph)
						(define-key map (kbd "M-k") 'forward-paragraph)
						(define-key map (kbd "M-j") 'left-word)
						(define-key map (kbd "M-l") 'right-word)

						(define-key map (kbd "C-M-j")  'windmove-left)
						(define-key map (kbd "C-M-l") 'windmove-right)
						(define-key map (kbd "C-M-i")    'windmove-up)
						(define-key map (kbd "C-M-k")  'windmove-down)

						;; delete 
						(define-key map (kbd "C-d") nil)
						(define-key map (kbd "C-d <down>") 'kill-line)
						(define-key map (kbd "C-d <up>") '(lambda () (interactive) (kill-line 0)))
						(define-key map (kbd "C-d <left>") 'delete-backward-char)
						(define-key map (kbd "C-d <right>") 'delete-forward-char)
						(define-key map (kbd "C-d C-e") 'kill-line)
						(define-key map (kbd "C-d C-a") '(lambda () (interactive) (kill-line 0)))
						(define-key map (kbd "C-S-d") 'kill-whole-line)
						
						;; miscellaneous actions
						(define-key map (kbd "C-s") 'save-buffer)
						(define-key map (kbd "C-S-s") 'save-some-buffers)
						(define-key map (kbd "C-x C-s") 'helm-occur)
						(define-key map (kbd "C-f") 'isearch-search)
						(define-key map (kbd "C-x C-a") 'mark-whole-buffer)
						(define-key map (kbd "C-p") 'recenter-top-bottom)
						(define-key map (kbd "M-q") 'keyboard-quit)
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
		(define-key map (kbd "M-S-i") 'scroll-up-command)
		(define-key map (kbd "M-S-i") 'scroll-down-command)
		(define-key map (kbd "C-o") 'point-redo)
		(define-key map (kbd "C-u") 'point-undo)
		(define-key map (kbd "M-u") 'goto-last-change)
		(define-key map (kbd "M-i") 'backward-paragraph)
		(define-key map (kbd "M-k") 'forward-paragraph)
		(define-key map (kbd "M-j") 'left-word)
		(define-key map (kbd "M-l") 'right-word)

		;; delete 
		(define-key map (kbd "C-d") nil)
		(define-key map (kbd "C-d <down>") 'kill-line)
		(define-key map (kbd "C-d <up>") '(lambda () (interactive) (kill-line 0)))
		(define-key map (kbd "C-d <left>") 'delete-backward-char)
		(define-key map (kbd "C-d <right") 'delete-forward-char)
		(define-key map (kbd "C-d C-e") 'kill-line)
		(define-key map (kbd "C-d C-a") '(lambda () (interactive) (kill-line 0)))
		(define-key map (kbd "C-S-d") 'kill-whole-line)

		(define-key map (kbd "M-q") 'keyboard-quit)

		))


(add-hook 'text-mode-hook 'underi-mode)
(add-hook 'prog-mode-hook 'underi-mode)

(provide 'underi-mode)

;;; underi-mode.el ends here

