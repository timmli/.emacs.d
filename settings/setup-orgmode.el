
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

(setq org-support-shift-select t)
;; (setq org-completion-use-ido t)
(setq org-src-fontify-natively t)


;; LaTeX support
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))

;; plantuml
;; http://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html
(setq org-plantuml-jar-path
      (expand-file-name "plantuml.jar" org-directory))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

;;==========================================================
;;      TAGS
;;==========================================================

(setq org-tag-alist '(("uni" . ?u)
											("privat" . ?p)
											("a02" . ?a)
											("lehre" . ?l)
											("forschung" . ?f)
											))

;;==========================================================
;;      KEYS
;;==========================================================


(global-set-key (kbd "<f9> c") 'org-goto-calendar)
(global-set-key (kbd "<f9> a") 'org-agenda-list)
(global-set-key (kbd "<f9> t") 'org-todo-list)
(global-set-key (kbd "C-<f9>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> s") 'org-search-view)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<tab>") nil ))



;; (bind-key "C-c r" 'org-capture)
;; (bind-key "C-c a" 'org-agenda)
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
