
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgfe5d909
;; http://doc.norang.ca/org-mode.html#Setup


;;==========================================================
;;      FILES
;;==========================================================

(setq org-directory (concat (getenv "HOME") "/Dropbox/Notizen/org"))
(setq org-agenda-files (list org-directory))


;;==========================================================
;;      MODULES
;;==========================================================

;; ;; See http://orgmode.org/worg/org-contrib/
;; (setq org-modules '( ;; org-bbdb
;;                       ;; org-gnus
;;                       ;; org-drill
;;                       ;; org-info
;;                       ;; org-jsinfo
;;                       ;; org-habit
;;                       ;; org-irc
;;                       ;; org-mouse
;;                       ;; org-protocol
;;                       ;; org-annotate-file
;;                       ;; org-eval
;;                       ;; org-expiry
;;                       ;; org-interactive-query
;;                       ;; org-man
;;                       ;; org-collector
;;                       ;; org-panel
;;                       ;; org-screen
;;                       ;; org-toc
;; 											))
;; (eval-after-load 'org
;;  '(org-load-modules-maybe t))
;; (setq org-expiry-inactive-timestamps t)


;; selection
(setq org-support-shift-select t)

;; source blocks
(setq org-src-fontify-natively t)

;; todo lists
(setq org-enforce-todo-dependencies t)

;; links
(setq org-return-follows-link t)

;; LaTeX support
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
(setq org-highlight-latex-and-related '(latex script entities)) ; inline sytax highlighting

;; plantuml
;; http://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html
(setq org-plantuml-jar-path
      (expand-file-name "plantuml.jar" org-directory))
(org-babel-do-load-languages  'org-babel-load-languages '((plantuml . t)))




;;==========================================================
;;      TAGS
;;==========================================================

(setq org-tag-alist '(("uni" . ?u)
											("privat" . ?p)
											("a02" . ?a)
											("lehre" . ?l)
											("forschung" . ?f)
											("emacs" . ?e)
											("latex" . ?x)
											))

;;==========================================================
;;      KEYS
;;==========================================================

(global-set-key (kbd "<f9> c") 'org-goto-calendar)
(global-set-key (kbd "<f9> a") 'org-agenda-list)
(global-set-key (kbd "<f9> t") 'org-todo-list)
(global-set-key (kbd "C-<f9>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> s") 'org-search-view)
(global-set-key (kbd "<f9> l") 'org-tags-view)
(global-set-key (kbd "<f9> r") 'org-reload)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<tab>") nil ))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "S-<up>") nil ))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "S-<down>") nil ))

;; (bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
;; (bind-key "C-c l" 'org-store-link)
;; (bind-key "C-c L" 'org-insert-link-global)
;; (bind-key "C-c O" 'org-open-at-point-global)
;; (bind-key "<f9> <f9>" 'org-agenda-list)
;; (bind-key "<f9> n" 'org-cycle-agenda-files)
;; (bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))

;; (with-eval-after-load 'org
;;   (bind-key "C-M-w" 'append-next-kill org-mode-map)
;;   (bind-key "C-TAB" 'org-cycle org-mode-map)
;;   (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
;;   (bind-key "C-c C-r" 'org-refile org-mode-map)
;;   (bind-key "C-c R" 'org-reveal org-mode-map)

;; (with-eval-after-load 'org-agenda
;;   (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))


;;==========================================================
;;      CAPTURE TEMPLATES
;;==========================================================

(setq org-capture-templates
      '(("t" "My TODO task format." entry
         (file (concat org-directory "/todo.org"))
         "* TODO %?\n SCHEDULED: %t")
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
				 "* %?\nEntered on %U\n  %i\n  %a"))
			)

(defun direct-org-task-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "t"))

(global-set-key (kbd "<f9> <f9> t") (lambda () (interactive) (org-capture nil "t")))
(global-set-key (kbd "<f9> <f9> j") (lambda () (interactive) (org-capture nil "j")))


;;==========================================================
;;      TODO KEYWORDS
;;==========================================================

;; (setq org-todo-keywords
;;       (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
;;               (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

;; (setq org-todo-keyword-faces
;;       (quote (("TODO" :foreground "red" :weight bold)
;;               ("NEXT" :foreground "blue" :weight bold)
;;               ("DONE" :foreground "forest green" :weight bold)
;;               ("WAITING" :foreground "orange" :weight bold)
;;               ("HOLD" :foreground "magenta" :weight bold)
;;               ("CANCELLED" :foreground "forest green" :weight bold)
;;               ("MEETING" :foreground "forest green" :weight bold)
;;               ("PHONE" :foreground "forest green" :weight bold))))


(provide 'setup-orgmode)
