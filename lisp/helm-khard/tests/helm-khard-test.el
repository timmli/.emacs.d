(let* ((helm-khard-test-dir (file-name-directory (buffer-file-name)))
			 (helm-khard-config-file (expand-file-name (concat helm-khard-test-dir
																												 "khard.conf")))
			 (helm-khard-vdirsyncer-command "echo \"Fake execution of vdirsyncer ...\"")
			 (helm-khard--candidates nil))
	(setenv "HELM_KHARD_TEST_DIR" helm-khard-test-dir)
	(require 'helm-khard)
	(helm-khard))
