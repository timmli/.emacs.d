;;; appearance --- General appearence of emacs

;; don't show toolbar
(tool-bar-mode 0)

;; brackets
(show-paren-mode 1)
(setq show-paren-delay 0)

;; no beep
(setq visible-bell nil)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; highlight line of cursor
(global-hl-line-mode t)

;; soft-wrap lines
(global-visual-line-mode t)

;; cursor
(blink-cursor-mode 1)										; blink
(setq blink-cursor-blinks 0)						; blink forever

;; ;; line numbers
;; (global-linum-mode t)
;; (setq linum-format " %3d ")

;; theme
(setq themes-dir
      (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path themes-dir)
(load-theme 'monokai t)

;; mode line
(use-package smart-mode-line
	:ensure t
	:init
	;; (setq sml/theme 'dark)
	(setq sml/no-confirm-load-theme t)
	:config
	(sml/setup)
	)

;; show file path in window title
(setq frame-title-format
      '(buffer-file-name "%b - %f" ; File buffer
        (dired-directory dired-directory ; Dired buffer
         (revert-buffer-function "%b" ; Buffer Menu
																 ("%b - Dir: " default-directory))))) ; Plain buffer

;; show date and time
(setq display-time-24hr-format t)
(display-time-mode +1)

;; fringe style
;; (set-face-attribute 'fringe nil :background "#3F3F3F" :foreground "#3F3F3F")

;; distraction-free mode
(use-package writeroom-mode
	:ensure t
	:bind
	(:map writeroom-mode-map
				("C-M-<" . writeroom-decrease-width)
				("C-M->" . writeroom-increase-width)
				("C-M-=" . writeroom-adjust-width)
				("C-<f10>" . writeroom-toggle-mode-line)
				)
	)
(global-set-key (kbd "<f10>") 'writeroom-mode)


;; toggle proportional mode when appropriate
;; inspired by https://ogbe.net/blog/toggle-serif.html
(defvar font-preserve-default-list nil
  "A list holding the faces that preserve the default family and
  height when TOGGLE-SERIF is used.")
(setq font-preserve-default-list
      '(;; LaTeX markup
        font-latex-math-face
        font-latex-sedate-face
        font-latex-warning-face
        ;; org markup
        org-latex-and-related
        org-meta-line
        org-verbatim
        org-block-begin-line
        ;; syntax highlighting using font-lock
        font-lock-builtin-face
        font-lock-comment-delimiter-face
        font-lock-comment-face
        font-lock-constant-face
        font-lock-doc-face
        font-lock-function-name-face
        font-lock-keyword-face
        font-lock-negation-char-face
        font-lock-preprocessor-face
        font-lock-regexp-grouping-backslash
        font-lock-regexp-grouping-construct
        font-lock-string-face
        font-lock-type-face
        font-lock-variable-name-face
        font-lock-warning-face))
(defun toggle-proportional ()
  "Change the default face of the current buffer to use a proportional family."
  (interactive)
  (when (display-graphic-p)  ;; this is only for graphical emacs
    ;; the serif font familiy and height, save the default attributes
    (let ((proportional-fam "Segoe UI")
          (proportional-height 125)
          (default-fam (face-attribute 'default :family))
          (default-height (face-attribute 'default :height)))
      (if (not (bound-and-true-p default-cookie))
          (progn (make-local-variable 'default-cookie)
                 (make-local-variable 'preserve-default-cookies-list)
                 (setq preserve-default-cookies-list nil)
                 ;; remap default face to serif
                 (setq default-cookie
                       (face-remap-add-relative
                        'default :family proportional-fam :height proportional-height))
                 ;; keep previously defined monospace fonts the same
                 (dolist (face font-preserve-default-list)
                   (add-to-list 'preserve-default-cookies-list
                                (face-remap-add-relative
                                 face :family default-fam :height default-height)))
                 (message "Turned on proportional font."))
        ;; undo changes
        (progn (face-remap-remove-relative default-cookie)
               (dolist (cookie preserve-default-cookies-list)
                 (face-remap-remove-relative cookie))
               (setq default-cookie nil)
               (setq preserve-default-cookies-list nil)
               (message "Restored default fonts."))))))

(provide 'appearance)
