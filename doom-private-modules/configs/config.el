;;; config.el -*- lexical-binding: t; -*-
;; (def-package! proof-general)

;; yas-snippets

;;; Code:
(defun my-try-yas-regex-expand (text regex-snippet)
  (interactive)
  (let* ((regex (car regex-snippet))
         (snippet (cdr regex-snippet))
         (matched-index (string-match regex text))
         (matched-buffer-index (if matched-index (+ (line-beginning-position) matched-index) nil)))
    (if matched-index
        (progn
          (setq my-regex-snippet-matched-string (buffer-substring-no-properties matched-buffer-index (point)))
          (let ((snippet (yas-lookup-snippet snippet nil t)))
            (if snippet
                (progn
                  (delete-region matched-buffer-index (point))
                  (yas-expand-snippet snippet nil nil '((yas-indent-line 'fixed)))
                  t))))
      nil)))

(defun my-yas-expand-regex (regex-snippets)
  (interactive)
  (let* ((text (buffer-substring-no-properties (line-beginning-position) (point)))
         (could-expand (or-map (apply-partially 'my-try-yas-regex-expand text) regex-snippets)))
    (if (not could-expand)
        (let* ((yas-buffer-local-condition '(require-snippet-condition . force-with-space-press))
               (cmd (yas-maybe-expand-abbrev-key-filter t)))
          (if cmd
              (yas-expand)
            (insert " "))))))

(defun or-map (my-fun sequence)
  (catch 'or-map-end
    (loop for i from 0 below (length sequence) do
          (if (funcall my-fun (aref sequence i))
              (throw 'or-map-end t)))
    nil))

(after! yas-minor-mode
  (setq yas-buffer-local-condition t)

  ;; TODO: This key-binding is not loaded by default
  (define-key yas-minor-mode-map (kbd "SPC")
    ;; Expand if snippet has condition: force-with-space-press
    (lambda ()
      (interactive)
      (my-yas-expand-regex [("[0-9]+//$" . "regex-frac")
                            ("[a|t|v|s][0-9]$" . "register")]))))
;; Notmuch
;;
(require 'notmuch)
(add-to-list 'load-path "/usr/share/org-mode/lisp")
(require 'org-notmuch)
(setq notmuch-search-oldest-first nil)

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-sent-folder        "/bak.skickat")
(setq mu4e-drafts-folder      "/bak.utkast")
(setq mu4e-trash-folder       "/bak.papperskorgen")
(setq smtpmail-smtp-user      "dv-sekreterare@utn.se")
(setq user-mail-address       "dv-sekreterare@utn.se")
(setq mu4e-compose-signature  "--
Leo Okawa Ericson
Sekreterare
Uppsala Datavetare (UD)
076-032 54 33
dv-sekreterare@utn.se

www.datavetenskap.nu
")
;;
;;
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
                           "~/org/orgzly/Projects.org"
                           "~/org/orgzly/begrepp.org"
                           "~/org/orgzly/schema.org"
                           "~/org/orgzly/Events.org"))

  ;; Start agenda on today
  (setq org-agenda-start-day "-0d")
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-capture-templates
        '(("w" "Weekly Review" entry (file+datetree "~/Dropbox/org/reviews.org")
           (file "~/Dropbox/org/templates/weeklyreviewtemplate.org"))
          ("d" "Daily Review" entry (file+datetree "~/Dropbox/org/reviews.org")
           (file "~/Dropbox/org/templates/dailyreviewtemplate.org"))
          ("m" "Mötesprotokoll" entry (file+headline "~/Dropbox/org/orgzly/Projects.org" "Sekreterare")
           "* NEXT Renskriva protokoll %t
styrelseprotokoll%?
* TODO Skriva ut protokoll"
           :jump-to-captured t
           )
          ("t" "Todo" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
           "* TODO %? ")
          ("f" "Todo" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
           "* TODO %?\n %a "
           :created t)))
 
  ;; (add-hook 'org-capture-mode-hook 'make-frame)
  (add-to-list 'org-modules 'org-habit))

(def-package! org-super-agenda
  :config
  (setq org-super-agenda-groups
        '((:log t)

          (:name "Tenta"
                 :tag "tenta"
                 :face (:background  "black":foreground"red" ))
          (:name "Inlämningar"
                 :tag "inlämning"
                 :face (:background "black" :foreground "orange" :underline t))
          (:name "Schema"
                 :tag "schema"
                 :face (:background "#527a45" :foreground "black" :underline t))
          (:name "Schedule"
                 ;; :time-grid t
                 :scheduled today)
          
          (:name "Events"
                 :tag "event"
                 :face (:background "#88bbf7" :foreground "black" :underline t))
          (:name "Habits"
                 :habit t)

          (:name "Scheduled earlier"
                 :scheduled past)
          (:name "Deadlines"
                 :deadline future
                 :face (:background  "black":foreground"red")
                 :order -1)))
  (org-super-agenda-mode)
  ;; Export all agenda fils to ical files in the directory ~/Dropbox/org/.export/
  ;; (org-icalendar-export-agenda-files)
  )

;; A stuck project is a project tagged with :project: and is at level two,
;; usually this means entries in Projects.org
(setq org-stuck-projects
      '("+LEVEL=2+project/-LATER-DONE-CANCELLED"
        ("NEXT")))
(defvar my-gtd-context-agenda-group
  '((:discard (:scheduled future))
    (:name "Next"
           :todo "NEXT")
    (:name "Todo"
           :todo "TODO"))
  "Discard all items that are scheduled for the future.")

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         (;; (tags "STYLE=\"habit\"")
          (todo ""
                ((org-super-agenda-groups
                  '((:name "Unscheduled nexts"
                           :and (:todo "NEXT"
                                       :scheduled nil))
                    (:discard (:anything t))))
                 ))
          (agenda "")))
        ("gc" "Computer" tags"+@computer/+TODO|+NEXT"
         ((org-agenda-overriding-header "At the computer")
          (org-super-agenda-groups my-gtd-context-agenda-group)
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("gh" "Home" tags"+@home/+TODO|+NEXT"
         ((org-agenda-overriding-header "At home")
          (org-super-agenda-groups my-gtd-context-agenda-group)
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("gm" "Styerelsemöte" tags"+@meeting"
         ((org-agenda-overriding-header "Styrelsemöte")
          (org-super-agenda-groups my-gtd-context-agenda-group)
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("d" "Daily review"
         (
          (;; Search inboxes for items that are not done
           search "*" ((org-super-agenda-groups nil)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED")))
                       (org-agenda-files
                        '("/home/leo/org/orgzly/Inbox.org"
                          "/home/leo/org/orgzly/InboxComputer.org"))))
          (todo "" ((org-super-agenda-groups
                     '((:name "Scheduled earlier"
                              :scheduled past)
                       (:discard (:anything t))))))
          (stuck "")
          (todo "TODO")))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (if (string-equal "Todo" (file-name-base (buffer-file-name)))
    nil
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max)))))))

(defun org-current-is-todo ()
  (let ((state (org-get-todo-state)))
    (some (lambda (x) (string= x state))
          '("TODO" "NEXT"))))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))


(setq org-log-into-drawer t)

(map! :after org
      :map org-mode-map
      :n "gr" #'leo/org-refile-hydra/body)

(map! :after magit
      :map magit-mode-map
      :n "%" #'magit-gitflow-popup)
(setq org-refile-targets
      '(("Projects.org"  :maxlevel . 2)
        ("Todo.org" :maxlevel . 1)
        ("Later.org" :maxlevel . 2)))

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
