

;; LaTeX stuff
;;
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
;; make LaTeXmk default
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (setq TeX-command-default "LaTeXmk")))

