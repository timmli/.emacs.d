
;;==========================================================
;;      KEYS
;;==========================================================

;; better keys for switching between windows
;; (when (fboundp 'windmove-default-keybindings)
;; (windmove-default-keybindings))
(global-set-key (kbd "M-S-<left>")  'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)
(global-set-key (kbd "M-S-<up>")    'windmove-up)
(global-set-key (kbd "M-S-<down>")  'windmove-down)

(global-set-key (kbd "M-s-<left>")  'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>")   'windmove-up)
(global-set-key (kbd "M-s-<down>")  'windmove-down)


(provide 'setup-frame)
