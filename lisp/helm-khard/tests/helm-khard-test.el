(let* ((helm-khard-test-dir (file-name-directory (buffer-file-name)))
			 (helm-khard-config-file (expand-file-name (concat helm-khard-test-dir
																												 "khard.conf")))
			 (helm-khard-vdirsyncer-command "echo \"Faking the execution of vdirsyncer ...\"")
			 (helm-khard--candidates nil)
			 (helm-khard-insert-with-organisation nil)
			 (helm-khard-sync-after-editing t)
			 (helm-khard-sync-during-initialization t))
	(setenv "HELM_KHARD_TEST_DIR" helm-khard-test-dir)
	(require 'helm-khard)
	(helm-khard)
	;; (helm-khard--search-candidates `(:name "Frank Dawson"))
	;; (helm-khard-edit-contact-action `(,(car helm-khard--candidates)))
	;; (helm-khard-insert-field-action (cdr (car helm-khard--candidates)))
	;; (let ((helm-khard--merge-ongoing t))(helm-khard))
	)
