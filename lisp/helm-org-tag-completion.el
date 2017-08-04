;;;;;; Fix Helm org tag completion
;; From Anders Johansson <https://groups.google.com/d/msg/emacs-helm/tA6cn6TUdRY/G1S3TIdzBwAJ>

;; This works great!  He posted it on 3 Mar 2016, on a thread that was
;; started in Oct 2013.  He also posted this message on 2 Apr 2014,
;; maybe an earlier attempt at a solution:
;; <http://article.gmane.org/gmane.emacs.orgmode/84495> I've just
;; tidied it up a bit and adjusted the prompt.

(add-to-list 'helm-completing-read-handlers-alist '(org-capture . aj/org-completing-read-tags))
(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . aj/org-completing-read-tags))

(defun aj/org-completing-read-tags (prompt coll pred req initial hist def inh)
  (if (not (string= "Tags: " prompt))
      ;; Not a tags prompt.  Use normal completion by calling
      ;; `org-icompleting-read' again without this function in
      ;; `helm-completing-read-handlers-alist'
      (let ((helm-completing-read-handlers-alist (rassq-delete-all
                                                  'aj/org-completing-read-tags
                                                  helm-completing-read-handlers-alist)))
        (org-icompleting-read prompt coll pred req initial hist def inh))
    ;; Tags prompt
    (let* ((initial (and (stringp initial)
                         (not (string= initial ""))
                         initial))
           (curr (when initial
                   (org-split-string initial ":")))
           (table (org-uniquify
                   (mapcar 'car org-last-tags-completion-table)))
           (table (if curr
                      ;; Remove current tags from list
                      (cl-delete-if (lambda (x)
                                      (member x curr))
                                    table)
                    table))
           (prompt (if initial
                       (concat "Tags " initial)
                     prompt)))
      (concat initial (mapconcat 'identity
                                 (nreverse (aj/helm-completing-read-multiple
                                            prompt table pred nil nil hist def
                                            t "Org tags" "*Helm org tags*" ":"))
                                 ":")))))

(defun aj/helm-completing-read-multiple (prompt choices
                                                &optional predicate require-match initial-input hist def
                                                inherit-input-method name buffer sentinel)
  "Read multiple items with `helm-completing-read-default-1'. Reading stops
when the user enters SENTINEL. By default, SENTINEL is
\"*done*\". SENTINEL is disambiguated with clashing completions
by appending _ to SENTINEL until it becomes unique. So if there
are multiple values that look like SENTINEL, the one with the
most _ at the end is the actual sentinel value. See
documentation for `ido-completing-read' for details on the
other parameters."
  (let ((sentinel (or sentinel "*done*"))
        this-choice res done-reading)
    ;; Uniquify the SENTINEL value
    (while (cl-find sentinel choices)
      (setq sentinel (concat sentinel "_")))
    (setq choices (cons sentinel choices))
    ;; Read choices
    (while (not done-reading)
      (setq this-choice (helm-completing-read-default-1 prompt choices
                                                        predicate require-match initial-input hist def
                                                        inherit-input-method name buffer nil t))
      (if (equal this-choice sentinel)
          (setq done-reading t)
        (setq res (cons this-choice res))
        (setq prompt (concat prompt this-choice ":"))))
    res))