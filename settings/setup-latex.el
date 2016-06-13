

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
(define-key LaTeX-mode-map (kbd "C-l C-q") 'align-current)

;; wrap quotes around active region
(defadvice TeX-insert-quote (around wrap-region activate)
      (cond
       (mark-active
        (let ((skeleton-end-newline nil))
          (skeleton-insert `(nil ,TeX-open-quote _ ,TeX-close-quote) -1)))
       ((looking-at (regexp-opt (list TeX-open-quote TeX-close-quote)))
        (forward-char (length TeX-open-quote)))
       (t
        ad-do-it)))
(put 'TeX-insert-quote 'delete-selection nil)
;; the same for single quotes
(defun TeX-insert-single-quote (arg)
	(interactive "p")
	(cond
	 (mark-active
		(let ((skeleton-end-newline nil))
			(skeleton-insert
			 `(nil ?` _ ?') -1)))
	 ((or (looking-at "\\<")
				(looking-back "^\\|\\s-\\|`"))
		(insert "`"))
	 (t
		(self-insert-command arg))))
(add-hook 'LaTeX-mode-hook
					'(lambda ()
						 (local-set-key "'" 'TeX-insert-single-quote)))

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


;; miscellaneous keys
(define-key LaTeX-mode-map (kbd "C-l <backspace>") 'TeX-clean)
(define-key LaTeX-mode-map (kbd "C-<return>") 'LaTeX-close-environment)

;; font keys
(defun TeX-italic()
  (interactive)
  (TeX-font nil ?\C-i))
(defun TeX-bold()
  (interactive)
  (TeX-font nil ?\C-b))
(defun TeX-typewriter()
  (interactive)
  (TeX-font nil ?\C-t))
(defun TeX-emphasis()
  (interactive)
  (TeX-font nil ?\C-e))
(defun TeX-smallcaps()
  (interactive)
  (TeX-font nil ?\C-c))
(define-key LaTeX-mode-map (kbd "C-l C-i") 'TeX-italic)
(define-key LaTeX-mode-map (kbd "C-l C-b") 'TeX-bold)
(define-key LaTeX-mode-map (kbd "C-l C-t") 'TeX-typewriter)
(define-key LaTeX-mode-map (kbd "C-l C-e") 'TeX-emphasis)
(define-key LaTeX-mode-map (kbd "C-l C-s") 'TeX-smallcaps)

(define-key LaTeX-mode-map (kbd "C-l C-a") 'LaTeX-find-matching-begin)
(define-key LaTeX-mode-map (kbd "C-l C-e") 'LaTeX-find-matching-end)

(provide 'setup-latex)
