;; automatically update buffers when files change
(global-auto-revert-mode t)

;; visible bell
(setq visible-bell t)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; delete marked text on typing
(delete-selection-mode t)

;; use tabs for indent
(setq-default tab-width 2)
(setq-default indent-tabs-mode t)

;; scrolling
(setq scroll-step            1
      scroll-conservatively  10000)

;; show vertical line per indentation level (BUG: highlight-indent-guides-mode: Wrong number of arguments: (2 . &rest), 1)
;; (require 'highlight-indent-guides)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; (setq highlight-indent-guides-method 'character)

;; yasnippet (before auto-complete)
(require 'yasnippet)
(yas-global-mode 1)

;; ;; auto-complete, sequence is important
;; (require 'auto-complete)
;; (require 'auto-complete-auctex)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-auto-show-menu t)
;; (setq ac-auto-show-menu 1)
;; (global-auto-complete-mode 1)
;; ;; (add-to-list 'ac-modes 'latex-mode)     ; activate auto-complete for latex <modes (AUCTeX or Emacs' builtin one).
;; (add-hook 'latex-mode-hook (function (lambda ()
;; 																					(ac-source-yasnippet))))

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-auctex)
(company-auctex-init)
;; yasnippet integration
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;; some general variables
(setq company-idle-delay 0.3
			company-minimum-prefix-length 1
			company-selection-wrap-around t
			;; company-show-numbers t
			company-dabbrev-downcase nil
			company-auto-complete nil
			company-transformers '(company-sort-by-occurrence))
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-active-map (kbd "TAB") 'company-select-next)
;;      (define-key company-active-map [tab] 'company-select-next)))
(with-eval-after-load 'company (company-flx-mode +1))

;; flycheck
(require 'flycheck)
(global-flycheck-mode t)

;; flyspell
(require 'setup-flyspell)

;; smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode-hook
          ruby-mode-hook
					emacs-lisp-mode-hook
					LaTeX-mode-hook
					TeX-mode-hook
          markdown-mode
          groovy-mode-hook
          scala-mode-hook)
  (add-hook it #'smartparens-mode))
(require 'smartparens-latex)

;; jump to matching paren
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))
(global-set-key (kbd "C-M-m") 'goto-match-paren)
(global-set-key (kbd "M-p") 'sp-backward-sexp)
(global-set-key (kbd "M-n") 'sp-forward-sexp)
(global-set-key (kbd "M-m") 'goto-match-paren)
(global-set-key (kbd "M-a") 'sp-beginning-of-sexp)
(global-set-key (kbd "M-e") 'sp-end-of-sexp)
(global-set-key (kbd "M-DEL") 'sp-unwrap-sexp)

;; https://ebzzry.github.io/emacs-pairs.html
;; (defmacro def-pairs (pairs)
;;   `(progn
;;      ,@(loop for (key . val) in pairs
;;           collect
;;             `(defun ,(read (concat
;;                             "wrap-with-"
;;                             (prin1-to-string key)
;;                             "s"))
;;                  (&optional arg)
;;                (interactive "p")
;;                (sp-wrap-with-pair ,val)))))
;; (def-pairs ((paren        . "(")
;;             (bracket      . "[")
;;             (brace        . "{")
;;             (single-quote . "'")
;;             (double-quote . "\"")
;;             (back-quote   . "`")))
;; (global-set-key (kbd "C-[") 'wrap-with-brackets) ; TODO: find nice key bindings
;; (global-set-key (kbd "C-(") 'wrap-with-parens)
;; (global-set-key (kbd "C-{") 'wrap-with-braces)

;; expand-region (intelligent selction)
(require 'expand-region)
(global-set-key (kbd "C-+") 'er/expand-region)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; cursor position history
(require 'point-undo)
(global-set-key [M-left] 'point-undo)
(global-set-key [M-right] 'point-redo)

;; cursor position undo history
(require 'goto-last-change)
(global-set-key (kbd "M-_") 'goto-last-change)

(defun my-markdown-mode-config ()
	"settings for markdown mode"
	(interactive)
	(setq-default tab-width 4)
	(setq-default indent-tabs-mode t)
	(setq markdown-enable-math t))
(add-hook 'markdown-mode 'my-markdown-mode-config)
(setq markdown-enable-math t)

;; adds support of the windows powershell
(require 'powershell)

;; switching between buffers with C-tab
(require 'iflipb)
(setq iflipb-wrap-around t)
(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)

;; adds ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; use deer instead plain directory listing
(require 'ranger)
(global-set-key (kbd "C-x C-d") 'deer)


;;==========================================================
;;      KEYS
;;==========================================================

;; commenting
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(eval-after-load "LaTeX-mode"
	'(define-key LaTeX-mode-map (kbd "C-;") 'comment-or-uncomment-region-or-line))
(eval-after-load "markdown-mode"
	'(define-key LaTeX-mode-map (kbd "C-;") 'comment-or-uncomment-region-or-line))
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
				(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

;; delete line
(global-set-key (kbd "C-S-o") 'delete-blank-lines)
(global-set-key (kbd "C-S-d") 'kill-whole-line)


;; center line
(global-set-key (kbd "C-S-l") 'recenter-top-bottom)

;; open untitiled new buffer
(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(global-set-key (kbd "<f7>") 'xah-new-empty-buffer)

(provide 'setup-buffer)
