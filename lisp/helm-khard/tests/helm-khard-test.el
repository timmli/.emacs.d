(require 'helm-khard)

(let ((helm-khard-config-file (expand-file-name (concat (file-name-directory (buffer-file-name))
																												"khard.conf")))
			(helm-khard-vdirsyncer-command "echo \"Fake execution of vdirsyncer ...\"")
			(helm-khard--candidates nil))
	(helm-khard))
