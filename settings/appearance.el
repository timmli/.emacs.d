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

;; line numbers
(global-linum-mode t)
(setq linum-format " %3d ")

;; theme
(setq themes-dir
      (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path themes-dir)
(load-theme 'monokai t)



(provide 'appearance)
