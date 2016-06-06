;; sr-speedbar
(require 'sr-speedbar)
(global-set-key (kbd "C-x C-k C-b") 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)                          ; always on left side
(add-hook 'speedbar-mode-hook '(lambda () (linum-mode 0))) ; disable linum for speedbar
(setq speedbar-show-unknown-files t)                       ; show all files
(setq sr-speedbar-width 30)                                ; default width


(provide 'setup-speedbar)
