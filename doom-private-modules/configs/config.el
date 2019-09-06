;;; config.el -*- lexical-binding: t; -*-
;; (def-package! proof-general)

(evil-ex-define-cmd "c" #'my-evil-copy)
(evil-define-command my-evil-copy (beg end address)
  "Copy lines in BEG END below line given by ADDRESS.
Usage: -3,+4c. for copying the range to point"
  :motion evil-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (forward-line -1)
    (insert txt)
    (forward-line -1)))
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
          ("m" "Mötesprotokoll" entry  (file+headline "~/Dropbox/org/orgzly/Projects.org" "Sekreterare")
           "* Protokoll
%t
** NEXT Renskriva protokoll %t :@computer
styrelseprotokoll%?
** WAITING Få godkänt av justerare %t :@computer:
** WAITING Få godkänt av ordförande %t :@computer:
** TODO Skriva ut protokoll  %t :@computer:
** TODO Ladda upp protokoll på drive %t :@computer:
  https://drive.google.com/drive/folders/1H_2oDyRqSyMhl_DubAwwcuCYxTRTPmvO"
           :jump-to-captured t
           )
          ("t" "Todo" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
           "* TODO %? ")
          ("f" "Todo" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
           "* TODO %?\n %a "
           :created t)
          ("x" "clipboard" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
           "* %?
%T
%x")))
 
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
(defconst my-project-default-next-task-states '("NEXT")
  "States that identifies which tasks in a project are considered next.")
(defvar my-project-next-task-states my-project-default-next-task-states
  "States that identifies which tasks in a project are considered next.")

(defun my-gtd-context-agenda-group ()
  `((:discard (:scheduled future :tag "think"))
    (:name "Project Next Task"
           :and (:file-path "Projects.org"
                            :and (:tag "focus"
                                       :todo ,my-project-next-task-states)))
    (:name "Unfocused Project Next Task"
           :order 4
           :face org-agenda-dimmed-todo-face
           :and (:file-path "Projects.org"
                            :and (:not (:tag "focus")
                                       :todo ,my-project-next-task-states)))
    (:name "Next"
           :and (:todo "NEXT"
                       :not (:file-path "Projects.org")))
    (:name "Todo"
           :and (:todo "TODO"
                       :not (:file-path "Projects.org")))
    (:discard (:anything t))))

(defun my-gtd-context-tag-agenda-settings (search-string desc)
  "Search for SEARCH-STRING with description DESC.
My settings for context agenda views that are based on the tags keyword."
  `((tags ,search-string
         ((org-agenda-overriding-header ,desc)
          (org-super-agenda-groups (my-gtd-context-agenda-group))))
   (stuck "" ((org-super-agenda-groups nil)))))


(setq org-agenda-custom-commands
      `(("c" "Simple agenda view"
         (;; (tags "STYLE=\"habit\"")
          (todo ""
                ((org-super-agenda-groups
                  '((:name "Unscheduled nexts"
                           :and (:todo "NEXT"
                                       :scheduled nil))
                    (:discard (:anything t))))
                 ))
          (agenda "")))
        ("p" "List of projects" stuck "" ((org-agenda-overriding-header "List of all projects. Tag those that you want to focus on with 'foucs'")
                                          (org-stuck-projects '("+LEVEL=2+project/-LATER-DONE-CANCELLED"
                                                                ("")))))
        ;; ("gc" "Computer" ,(my-gtd-context-tag-agenda-settings "+@computer/+NEXT|+TODO" "At the computer"))
        ("gh" "Home" ,(my-gtd-context-tag-agenda-settings "+@computer|+@home/+NEXT|+TODO" "At home"))
        ("gf" "Foobar" ,(my-gtd-context-tag-agenda-settings "+@computer|+@school|@foobar/+NEXT|+TODO" "In foobar"))
        ("gm" "Styerelsemöte" tags"+@meeting"
         ((org-agenda-overriding-header "Styrelsemöte")
          (org-super-agenda-groups nil)))
        ("d" "Daily review"
         (
          (;; Search inboxes for items that are not done
           search "*" ((org-agenda-overriding-header
                        "Empty Inbox!
\t- Tag stuff that is worth thinking about with 'think'
\t- Refile project tasks to their corresponding target
\t- Tag stuff that should be scheduled later than a week forwards with 'weeklyschedule'
\t- Tag stuff that needs to be review with 'weeklyreview'
\t- Check inbox in drawer")
                       (org-super-agenda-groups nil)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED")))
                       (org-agenda-files
                        '("/home/leo/org/orgzly/Inbox.org"
                          "/home/leo/org/orgzly/InboxComputer.org"))))
          (todo "" ((org-super-agenda-groups
                     '((:name "Daily Review is done! Congratulations!"
                              :and (:heading-regexp "Daily review"
                                                    :tag "review")
                              :order 100)
                       (:name "Habits"
                              :habit t
                              :order 2)
                       (:name "Scheduled earlier"
                              :scheduled past
                              :order 1)
                       (:name "Review Todos\n\tSet tags\n\tChange relevant tasks to NEXT"
                              :and (:todo ("TODO")
                                          :file-path "Todo.org")
                              :order 3)
                       (:name "Review Nexts\n\tSet tags\n\tChange irrelevant tasks to TODO"
                              :and (:todo ("NEXT")
                                          :file-path "Todo.org")
                              :order 4)
                       (:discard (:anything t))))))
          (agenda "")
          (stuck "")))
        ("w" "Weekly Review"
         ((todo "" ((org-agenda-files (cons "/home/leo/org/orgzly/Later.org" org-agenda-files))
                    (org-super-agenda-groups
                     '((:name "Clock in!"
                              :and (:tag "weeklyreview"
                                         :heading-regexp "Weekly review"))
                       (:name "Review Later or Waiting tasks\n\tCan they be done this week?"
                              :todo ("WAITING" "LATER")
                              :file-path "Later.org"
                              :order 5)
                       (:name "Schedule these tasks for next week"
                              :and (:not (:todo ("DONE" "CANCELLED"))
                                         :tag "weeklyschedule")
                              :order 5)
                       (:name "Tasks related to weekly review"
                              :and (:not (:todo ("DONE" "CANCELLED"))
                                         :tag "weeklyreview")
                              :order 1)
                       (:name "Review Next tasks\n\tCan they be done this week?"
                              :and (:todo "NEXT"
                                          :file-path "Todo.org")
                              :order 3)
                       (:name "Which tasks are Next?"
                              :and (:todo "TODO"
                                          :not (:scheduled future)
                                          :not (:tag "project")
                                          :not (:habit t))
                              :order 4)
                       (:discard (:tag "project"))))))
          (stuck "" ((org-agenda-overriding-header "List of all projects. Tag those that you want to focus on with 'foucs'")
                     (org-stuck-projects '("+LEVEL=2+project/-LATER-DONE-CANCELLED"
                                           ("")))
                     (org-super-agenda-groups
                      '((:name "Focused"
                               :tag "focus")
                        (:name "Not Focused"
                               :anything t)))))
          (todo "" ((org-agenda-files '("/home/leo/org/orgzly/Projects.org"))
                    (org-agenda-overriding-header "Clock Out!")
                    (org-super-agenda-groups
                     '((:name ""
                              :and (:heading-regexp "Weekly review"
                                                    :tag "weeklyreview"))
                       (:discard (:anything t
                                            :habit ))))))))))


(defun my-restrict-org-todo-list ()
  (interactive)
  (let ((temp-states my-project-next-task-states))
    (setq my-project-next-task-states '("NEXT" "TODO"))
    (let* ((marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      (with-current-buffer buffer
        (goto-char pos)
        (outline-up-heading 1 t)
        (org-agenda-set-restriction-lock)))
    ;; (org-todo-list)
    (setq my-project-next-task-states
          my-project-default-next-task-states)))




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
;; Disable counsel-org-capture
(global-set-key [remap org-capture] nil)
(map! :after org
      (:map org-super-agenda-header-map
        :map org-super-agenda-header-map
        "j" #'org-agenda-next-line
        "k" #'org-agenda-previous-line
        :map org-agenda-keymap
        :map org-agenda-mode-map
        "k" #'org-agenda-previous-line
        "j" #'org-agenda-next-line
        "<" #'my-restrict-org-todo-list
        ">" #'org-agenda-remove-restriction-lock)
      :map org-mode-map
      :n "gr" #'leo/org-refile-hydra/body
      :m "[]" #'outline-up-heading)

(map! :after magit
      :map magit-mode-map
      :n "%" #'magit-gitflow-popup)
(setq org-refile-targets
      (append '(("Projects.org"  :maxlevel . 2)
                ("Todo.org" :maxlevel . 1)
                ("Later.org" :maxlevel . 2))
              (mapcar (lambda (x) (cons x '(:maxlevel . 1)))
                      (file-expand-wildcards "~/org/references/notes/*"))))

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
