;; sr-speedbar
(use-package sr-speedbar
	:ensure t
	:bind
	("C-c C-k C-b" . sr-speedbar-toggle)
	:config
	(setq sr-speedbar-right-side nil)                          ; always on left side
	(add-hook 'speedbar-mode-hook '(lambda () (linum-mode 0))) ; disable linum for speedbar
	(setq speedbar-show-unknown-files t)                       ; show all files
	(setq sr-speedbar-width 30)                                ; default width
	)


(provide 'setup-speedbar)
