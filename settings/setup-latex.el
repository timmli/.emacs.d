

;; to activate auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil) ;;autosave before compiling

;; reftex
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (require 'reftex)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;;(setq reftex-toc-split-windows-horizontally t)
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;; reftex config for beamer
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

;; make LaTeXmk default
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(setq TeX-command-force "LatexMk")

;; useful command to align arrays
(add-hook 'LaTeX-mode-hook
					(function
					 (lambda ()
						 (define-key LaTeX-mode-map (kbd "C-c a")
							 'align-current))))

;; keys for error browsing
(define-key LaTeX-mode-map (kbd "<f4>") 'TeX-next-error)
(define-key LaTeX-mode-map (kbd "S-<f4>") 'TeX-previous-error)
(define-key LaTeX-mode-map (kbd "C-<f4>") 'TeX-error-overview)

;; viewer
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-view-program-list
   '(("Sumatra PDF" ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                      (mode-io-correlate " -forward-search %b %n ") " %o"))))
(eval-after-load 'tex
  '(progn
     (assq-delete-all 'output-pdf TeX-view-program-selection)
     (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

(provide 'setup-latex)
