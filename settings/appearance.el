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

(provide 'appearance)
