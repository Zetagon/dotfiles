;;; private/bindings/config.el -*- lexical-binding: t; -*-

(after! org
  (setq org-agenda-files '("~/org/orgzly/Todo.org"
                           "~/org/orgzly/Inbox.org"
                           "~/org/orgzly/skolarbete.org"
                           "~/org/orgzly/begrepp.org"))
  (add-to-list 'org-modules 'org-habit))

(map! :after magit
      :map magit-mode-map
      :n "%" #'magit-gitflow-popup)
