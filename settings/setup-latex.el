

;; to activate auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (require 'reftex)))

;; beamer
(eval-after-load "tex"
  '(TeX-add-style-hook "beamer" 'my-beamer-mode))
(defun my-beamer-mode ()
  (require 'reftex)
  (set (make-local-variable 'reftex-section-levels)
       '(("section" . 1)
	 ("subsection" . 2)
	 ("frametitle" . 3)))
  (reftex-reset-mode)
  )

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;;(setq reftex-toc-split-windows-horizontally t)

;; make LaTeXmk default
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (setq TeX-command-default "LaTeXmk")))

;; useful command to align arrays
(add-hook 'LaTeX-mode-hook
					(function
					 (lambda ()
						 (define-key LaTeX-mode-map (kbd "C-c a")
							 'align-current))))

(provide 'setup-latex)
