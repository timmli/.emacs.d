

;; to activate auctex
(setq TeX-auto-save t)  
(setq TeX-auto-local
			(expand-file-name "temp" user-emacs-directory))
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil) ;;autosave before compiling

;; don't indent
;; (setq LaTeX-indent-level 0)
;; (setq LaTeX-item-indent 0)

;; make LaTeXmk default
(use-package auctex-latexmk
	:ensure t
	:config
	(auctex-latexmk-setup)
	(setq auctex-latexmk-inherit-TeX-PDF-mode t)
	(setq TeX-command-force "LatexMk")  ;; remember to set path variable accordingly!
	)

;; reftex
(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
				;;reftex-toc-split-windows-horizontally t
				reftex-ref-macro-prompt nil			; go straight to the labels when referencing
				reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")
        ;; reftex-default-bibliography '()
				)
	
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

	;; connect reftex to imenu
	(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
	(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
	
  :bind (:map LaTeX-mode-map
							("C-c ]" . reftex-citation)) ; same as in org-mode
	)

;; ;; ivy-bibtex
;; (use-package ivy-bibtex
;; 	:ensure t
;; 	:config
;; 	;; (setq bibtex-completion-bibliography '("./references.bib"))
;; 	(setq bibtex-completion-additional-search-fields '(bibtexkey))
;; 	;; (define-key LaTeX-mode-map (kbd "C-l C-r") 'ivy-bibtex)
;; 	;; The standard function with modified default action  
;; 	(defun ivy-bibtex (&optional arg)
;; 		"Search BibTeX entries using ivy. With a prefix ARG the cache is invalidated and the bibliography reread."
;; 		(interactive "P")
;; 		(when arg
;; 			(setq bibtex-completion-bibliography-hash ""))
;; 		(bibtex-completion-init)
;; 		(ivy-read "BibTeX Items: "
;; 							(bibtex-completion-candidates 'ivy-bibtex-candidates-formatter)
;; 							:caller 'ivy-bibtex
;; 							:action 'bibtex-completion-insert-key))
;; 	;; look for local bibliographies
;; 	;; (require 'ebib)
;; 	(defun ivy-bibtex-with-local-bibliography ()
;;     (interactive)
;;     (let ((bibtex-completion-bibliography
;; 					 (if (eq major-mode 'latex-mode)
;; 							 ;; (ebib--get-local-databases)
;; 							 (bibtex-completion--get-local-databases)
;; 						 bibtex-completion-bibliography)))
;; 			(ivy-bibtex)))
;; 	;; proposal by jagrg: https://github.com/tmalsburg/helm-bibtex/issues/112 
;; 	(defun bibtex-completion--get-local-databases ()
;; 		"Return a list of .bib files associated with the current file."
;; 		(let ((texfile nil)
;; 					(cb (current-buffer)))
;; 			(when (and (boundp 'TeX-master)
;; 								 (stringp TeX-master))
;; 				(setq texfile (if (file-name-extension TeX-master)
;; 													TeX-master
;; 												(concat TeX-master ".tex"))))
;; 			(with-temp-buffer
;; 				(if (and texfile (file-readable-p texfile))
;; 						(insert-file-contents texfile)
;; 					(insert-buffer-substring cb))
;; 				(save-match-data
;; 					(goto-char (point-min))
;; 					(cond
;; 					 ;; bibtex
;; 					 ((re-search-forward "\\\\\\(?:no\\)*bibliography{\\(.*?\\)}" nil t)
;; 						(mapcar (lambda (fname)
;; 											(if (file-name-extension fname)
;; 													fname
;; 												(concat fname ".bib")))
;; 										(split-string (match-string-no-properties 1) ",[ ]*")))
;; 					 ;; biblatex
;; 					 ((re-search-forward "\\\\addbibresource\\(\\[.*?\\]\\)?{\\(.*?\\)}" nil t)
;; 						(mapcar (lambda (fname)
;; 											(if (file-name-extension fname)
;; 													fname
;; 												(concat fname ".bib")))
;; 										(let ((option (match-string 1))
;; 													(file (match-string-no-properties 2)))
;; 											(unless (and option (string-match-p "location=remote" option))
;; 												(split-string file ",[ ]*")))))
;; 					 (t
;; 						bibtex-completion-bibliography))))))
;; 	:bind (:map LaTeX-mode-map 
;; 							("C-l C-r" . ivy-bibtex-with-local-bibliography))
;; 	)

;; see  pull request: https://github.com/tmalsburg/helm-bibtex/pull/113
;; ;; helm-bibtex FIXME: 
;; (use-package helm-bibtex
;; 	:ensure t
;; 	:config
;; 	;; (setq bibtex-completion-bibliography '("./references.bib"))
;; 	(setq bibtex-completion-additional-search-fields '(bibtexkey))
;; 	;; The standard function with modified default action  
;; 	;; :bind (:map LaTeX-mode-map ("C-l C-r" . helm-bibtex-with-local-bibliography))
;; 	)



;; useful command to align arrays
(define-key LaTeX-mode-map (kbd "C-l C-q") 'align-current)

;; ;; wrap quotes around active region (use smartparens instead)
;; (defadvice TeX-insert-quote (around wrap-region activate)
;;       (cond
;;        (mark-active
;;         (let ((skeleton-end-newline nil))
;;           (skeleton-insert `(nil ,TeX-open-quote _ ,TeX-close-quote) -1)))
;;        ((looking-at (regexp-opt (list TeX-open-quote TeX-close-quote)))
;;         (forward-char (length TeX-open-quote)))
;;        (t
;;         ad-do-it)))
;; (put 'TeX-insert-quote 'delete-selection nil)
;; ;; the same for single quotes
;; (defun TeX-insert-single-quote (arg)
;; 	(interactive "p")
;; 	(cond
;; 	 (mark-active
;; 		(let ((skeleton-end-newline nil))
;; 			(skeleton-insert
;; 			 `(nil ?` _ ?') -1)))
;; 	 ((or (looking-at "\\<")
;; 				(looking-back "^\\|\\s-\\|`"))
;; 		(insert "`"))
;; 	 (t
;; 		(self-insert-command arg))))
;; (add-hook 'LaTeX-mode-hook
;; 					'(lambda ()
;; 						 (local-set-key "'" 'TeX-insert-single-quote)))

;; keys for error browsing
(define-key LaTeX-mode-map (kbd "<f4>") 'TeX-next-error)
(define-key LaTeX-mode-map (kbd "S-<f4>") 'TeX-previous-error)
(define-key LaTeX-mode-map (kbd "C-<f4>") 'TeX-error-overview)
																				;
;; jumping around like in org-mode
(eval-after-load 'tex
	'(progn 
		 (define-key LaTeX-mode-map (kbd "C-c C-j") 'tl/reftex-in-follow-mode)
		 (define-key LaTeX-mode-map (kbd "C-c C-n") 'tl/reftex-next)
		 (define-key LaTeX-mode-map (kbd "C-c C-p") 'tl/reftex-previous)))
(defun tl/reftex-in-follow-mode()
	(interactive)
	(setq reftex-toc-follow-mode t)
	(reftex-toc))
(defun tl/reftex-next ()
	(interactive)
	(next-line)														; no clue why this is necessary
	(tl/reftex-in-follow-mode)
	(reftex-toc-next)
	(reftex-toc-goto-line-and-hide)
	(recenter))
(defun tl/reftex-previous ()
	(interactive)
	(next-line)														; no clue why this is necessary
	(tl/reftex-in-follow-mode)
	(reftex-toc-previous)
	(reftex-toc-goto-line-and-hide)
	(recenter))

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

;; make square brackets indent correctly (testing)
;(modify-syntax-entry ?\[ "(]" LaTeX-mode-syntax-table)
;(modify-syntax-entry ?\] ")[" LaTeX-mode-syntax-table)

(provide 'setup-latex)
