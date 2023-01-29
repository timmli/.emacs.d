;;; Minimal setup to load latest `org-mode'.

;; Activate debugging.
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; Add latest Org mode to load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/org-9.6.1"))


(defun tl/remove-org-timestamps ()
	"Remove all (exportet) Org timestamps and surrounding blanks from buffer."
	(save-excursion
		(while (re-search-forward
						(concat "\\([[:blank:]]\\)*"
										"\\(" org-element--timestamp-regexp "\\)"
										"\\(–\\|[[:blank:]]\\)*"
										) nil t)
			(replace-match ""))))

(defun tl/org-export-agenda-to-ics ()
	(interactive)
	(if (org-agenda-files)
			(let* (
						 (org-export-with-broken-links t) ; Ignore broken links
						 )
				(org-icalendar-combine-agenda-files) 
				(message (concat "org-agenda exported to "  org-icalendar-combined-agenda-file))
				(save-window-excursion
					(find-file org-icalendar-combined-agenda-file)
					(beginning-of-buffer)
					(tl/remove-org-timestamps)
					(save-buffer)
					(kill-buffer))
				(message (concat "org-agenda calendar file cleaned and saved: " org-icalendar-combined-agenda-file)))
		(message "tl/org-export-agenda-to-ics: org-agenda-files not specified, export aborted.")
		)
	)

(defun tl/org-export-buffer-to-ics ()
	(interactive)
	(if (buffer-file-name (current-buffer))
			(let* ((file-name (buffer-file-name (current-buffer)))
						 (org-agenda-files (list file-name))
						 (org-icalendar-combined-agenda-file
							(concat
							 (file-name-sans-extension file-name)
							 ".ics")))
				(tl/org-export-agenda-to-ics))
		(message "tl/org-export-buffer-to-ics: buffer has no file, export aborted.")
		))


(setq org-icalendar-timezone "Europe/Berlin"
			org-icalendar-include-todo nil			; Non-nil means create VTODO components from TODO items.
			org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due) ; Export all deadline time stamps.
			org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo) ; Export all scheduled time stamps.
			org-icalendar-include-body nil		; Amount of text below headline to be included in iCalendar export.
			)
