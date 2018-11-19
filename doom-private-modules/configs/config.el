;;; config.el -*- lexical-binding: t; -*-
;; (def-package! proof-general)
(map! :after proof
      :map proof-mode-map
      :n ":" #'proof-assert-next-command-interactive
      :i "." (λ!
                 (insert ".")
                 (proof-goto-point)))
(after! org
  (setq org-agenda-files '("~/org/orgzly/Todo.org"
                           "~/org/orgzly/Inbox.org"
                           "~/org/orgzly/skolarbete.org"
                           "~/org/orgzly/begrepp.org"
                           "~/org/orgzly/schema.org"))

  ;; Start agenda on today
  (setq org-agenda-start-day "-0d")
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-capture-templates
        '(("w" "Weekly Review" entry (file+datetree "~/Dropbox/org/reviews.org")
           (file "~/Dropbox/org/templates/weeklyreviewtemplate.org"))
          ("d" "Daily Review" entry (file+datetree "~/Dropbox/org/reviews.org")
           (file "~/Dropbox/org/templates/dailyreviewtemplate.org"))))
  (add-hook 'org-capture-mode-hook 'make-frame)
  (add-to-list 'org-modules 'org-habit))

(def-package! org-super-agenda
  :config
  (setq org-super-agenda-groups
        '((:log t)

          (:name "Schema"
                 :tag "schema"
                 :face (:background "#527a45" :foreground "black" :underline t))
          (:name "Inlämningar"
                 :tag "inlämning"
                 :face (:background "black" :foreground "orange" :underline t))
          (:name "Schedule"
                 :time-grid t
                 :scheduled today)
          (:name "Tenta"
                 :tag "tenta"
                 :face (:background  "black":foreground"red" ))
          (:name "Habits"
                 :habit t)
          (:name "Scheduled earlier"
                 :scheduled past)))
  (org-super-agenda-mode)
  ;; Export all agenda fils to ical files in the directory ~/Dropbox/org/.export/
  (org-icalendar-export-agenda-files))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((alltodo "")
          (agenda "")))))

(setq org-log-into-drawer t)

(map! :after org
      :map org-mode-map
      :n "gr" #'leo/org-refile-hydra/body)

(map! :after magit
      :map magit-mode-map
      :n "%" #'magit-gitflow-popup)

(defhydra leo/org-refile-hydra (:foreign-keys run)
  "Refile"
  ("s" leo/org-refile-hydra-skolarbete/body "Skolarbete.org" :exit t)
  ("t" leo/org-refile-hydra-todo/body "Todo.org" :exit t)
  ("l" leo/org-refile-hydra-later/body "Later.org" :exit t)
  ("q" nil "cancel"))

(defhydra leo/org-refile-hydra-skolarbete
  (:color blue :after-exit (leo/org-refile-hydra/body))
  "Skolarbete.org"
  ("t" (my/refile "skolarbete.org" "Tenta") "Tenta")
  ("i" (my/refile "skolarbete.org" "Inlämning") "Inlämning")
  ("q" nil "cancel"))

(defhydra leo/org-refile-hydra-todo
  (:color blue :after-exit (leo/org-refile-hydra/body))
  "Inbox.org"
  ("f" (my/refile "Todo.org" "Fritid") "Fritid")
  ("s" (my/refile "Todo.org" "Skola") "Skola")
  ("m" (my/refile "Todo.org" "Misc.") "Misc.")
  ("a" (my/refile "Todo.org" "Anki") "Anki")
  ("h" (my/refile "Todo.org" "Habits") "Habits")
  ("q" nil "cancel"))

(defhydra leo/org-refile-hydra-later
  (:color blue :after-exit (leo/org-refile-hydra/body))
  "Inbox.org"
  ("h" (my/refile "Later.org" "Hobby") "Hobby")
  ("l" (my/refile "Later.org" "Läsa") "Läsa")
  ("m" (my/refile "Later.org" "Misc.") "Misc.")
  ("e" (my/refile "Later.org" "Emacs tips.") "Emacs tips")
  ("q" nil "cancel"))

;;Example; (my/refile "someday-maybe.org" "Someday/Maybe")
(defun my/refile (file headline &optional arg)
  "Refile to file FILE under the headline HEADLINE."
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(provide 'config)
;;; config.el ends here
