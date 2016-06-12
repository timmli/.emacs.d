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

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-auctex)
(global-auto-complete-mode 1)
;; (ac-config-default)
;; (add-to-list 'ac-modes 'latex-mode)     ; activate auto-complete for latex <modes (AUCTeX or Emacs' builtin one).

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; flycheck
(require 'flycheck)
(global-flycheck-mode t)

;; flyspell
(setq ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
(global-set-key (kbd "<f6>") 'flyspell-buffer)
(global-set-key (kbd "M-<f6>") 'ispell-word)
(global-set-key (kbd "C-<f6>") 'flyspell-goto-next-error)
(global-set-key (kbd "C-S-<f6>") 'flyspell-goto-previous-error)
;; remove keybinding for autocorrect
(add-hook 'flyspell-mode-hook (function (lambda ()
																					(local-unset-key (kbd "C-;")))))
(add-hook 'flyspell-mode-hook (function (lambda ()
																					(local-unset-key (kbd "C-.")))))
;; (eval-after-load "flyspell"
;; 	'(dolist
;; 			 (define-key flyspell-mode-map (kbd "C-;") nil)
;; 		 (define-key flyspell-mode-map (kbd "C-.") nil)))
;; ;; activate for text
;; (dolist (hook '(text-mode-hook LaTeX-mode-hook))
;; 	(add-hook hook (lambda () (flyspell-mode 1))))
;; move point to previous error
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))))))

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

;; expand-region (intelligent selction)
(require 'expand-region)
(global-set-key (kbd "C-+") 'er/expand-region)

;; cursor position history
(require 'point-undo)
(global-set-key [M-left] 'point-undo)
(global-set-key [M-right] 'point-redo)

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
(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)

;; adds ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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




(provide 'setup-buffer)
