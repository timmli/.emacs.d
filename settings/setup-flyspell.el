;; flyspell
(setq ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
(global-set-key (kbd "C-<f6>") 'flyspell-toggle)
(global-set-key (kbd "<f6>") 'flyspell-toggle)
(eval-after-load "flyspell"
	'(dolist
			 (define-key flyspell-mode-map (kbd "M-<f6>") 'ispell-word)
		 (define-key flyspell-mode-map (kbd "<f6>") 'flyspell-next-and-ispell-word)
		 (define-key flyspell-mode-map (kbd "S-<f6>") 'flyspell-previous-and-ispell-word)))

;; remove keybindings for autocorrect 
(eval-after-load "flyspell"
	'(define-key flyspell-mode-map (kbd "C-;") nil))
(eval-after-load "flyspell"
	'(define-key flyspell-mode-map (kbd "C-.") nil))

;; ;; activate for text
;; (dolist (hook '(text-mode-hook LaTeX-mode-hook))
;; 	(add-hook hook (lambda () (flyspell-mode 1))))

(defun flyspell-toggle (arg)
	(interactive "p")
	(if (bound-and-true-p flyspell-mode)
			(progn
				 (flyspell-mode -1)
			)	 
		(progn
			(flyspell-buffer)
			(flyspell-mode)
			)))

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


(defun flyspell-next-and-ispell-word (args)
  ""
  (interactive "P")
	(progn
		(flyspell-goto-next-error)
		(ispell-word)
		)
  )

(defun flyspell-previous-and-ispell-word (args)
  ""
  (interactive "P")
	(progn
		(flyspell-goto-previous-error)
		(ispell-word)
		)
  )


(provide 'setup-flyspell)
