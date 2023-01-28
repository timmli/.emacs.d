;;; Minimal setup to load latest `org-mode'.

;; Activate debugging.
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; Add latest Org mode to load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/org-9.6.1"))


(defun tl/org-export-buffer-to-ics ()
	(interactive)
	(if (buffer-file-name (current-buffer))
			(let* ((file-name (buffer-file-name (current-buffer)))
						 (org-agenda-files (list file-name))
						 (org-icalendar-combined-agenda-file
							(concat
							 (file-name-sans-extension file-name)
							 ".ics")))
				(org-icalendar-combine-agenda-files))
		(message "tl/org-export-buffer-to-ics: buffer has no file, export aborted.")
		))

(setq org-export-with-broken-links t)
(setq org-icalendar-timezone "Europe/Berlin"
			org-icalendar-include-todo nil			; Non-nil means create VTODO components from TODO items.
			org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due) ; Export all deadline time stamps.
			org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo) ; Export all scheduled time stamps.
			org-icalendar-include-body nil		; Amount of text below headline to be included in iCalendar export.
			org-export-with-broken-links t		; Ignore broken links
			)
