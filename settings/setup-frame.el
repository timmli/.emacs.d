
;;==========================================================
;;      KEYS
;;==========================================================

;; better keys for switching between windows
;; (when (fboundp 'windmove-default-keybindings)
;; (windmove-default-keybindings))
(global-set-key (kbd "M-s-<left>")  'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>")   'windmove-up)
(global-set-key (kbd "M-s-<down>")  'windmove-down)

(global-set-key (kbd "<f2> <left>")  'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)
(global-set-key (kbd "<f2> <up>")   'windmove-up)
(global-set-key (kbd "<f2> <down>")  'windmove-down)

;; clone frame
(global-set-key (kbd "<f2> <f2>") 'make-frame)

;; split window
(global-set-key (kbd "<f2> v") 'split-window-vertically)
(global-set-key (kbd "<f2> h") 'split-window-horizontally)
(global-set-key (kbd "<f2> b") 'split-window-below)
(global-set-key (kbd "<f2> r") 'split-window-right)

;; close window
(global-set-key (kbd "M-<f2>") 'delete-window)

;; winner mode:
(winner-mode 1)
(global-set-key (kbd "<f2> z") 'winner-undo) ; undo pane configuration
(global-set-key (kbd "<f2> Z") 'winner-redo) ; redo pane configuration

(provide 'setup-frame)
