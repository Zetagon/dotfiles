;;; packages.el --- my-spacemacs-init layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Leo <leo@leo-B85-HD3>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-spacemacs-init-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-spacemacs-init/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-spacemacs-init/pre-init-PACKAGE' and/or
;;   `my-spacemacs-init/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-spacemacs-init-packages
  '()
  "The list of Lisp packages required by the my-spacemacs-init layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")



(defun my-user-config ()
                                        ;copied from the internet
  (defun increment-number-at-point ()
    (interactive)
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))) (require 'org)
  (global-set-key (kbd "C-c +") 'increment-number-at-point)

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-agenda-files '( "~/Dropbox/org/orgzly/skolarbete.org" "~/Dropbox/org/orgzly/Todo.org" "~/Dropbox/org/orgzly/begrepp.org" "~/Dropbox/org/references/articles.org"))
  (setq org-refile-targets (quote (("~/org/orgzly/skola.org" :maxlevel . 1)
                                   ("~/org/orgzly/Todo.org" :level . 1)
                                   ("~/org/brain/programming.org" :level . 1)
                                   ("~/org/orgzly/someday.org" :maxlevel . 1))))

  "Org todo keywords"
  (setq org-todo-keywords
        '((sequence "SOMEDAY""TODO" "IN PROGRESS"  "|" "DONE" "CANCELLED")))

  (setq org-capture-templates
        '(("l" "Ny läxa" entry
           (file "skola.org")
           "* Läxa %?
    SCHEDULED: %t")
          ("t" "TODO" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?
   Added: %T "
           )
          ("r" "To Read" entry (file+headline "~/org/someday.org" "To Read")
           "* %?
                 added: %t
                 "
           )
          ("b" "Nytt begrepp" entry
           (file "begrepp.org")
           "* begrepp 
%?
** Answer")
          ))

  (setq bookmark-default-file "~/Dropbox/emacsbookmarks")
  ;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
  ;; (add-to-list 'projectile-globally-ignored-directories "data")

  "Remap j and k to function with softwraps"
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "Ö") 'evil-ex)
  (define-key evil-normal-state-map (kbd "¤") (kbd "$"))
  (define-key evil-normal-state-map (kbd "\'") 'evil-goto-mark)
  (define-key evil-normal-state-map (kbd ",;r") 'jump-to-register)
  (set-register ?I '(file . "~/Dropbox/org/skola.org" ))
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'prog-mode-hook (lambda () (spacemacs/toggle-spelling-checking-off)))
  
        
  ;;Mu4e

  (setq mu4e-maildir "~/.mail"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-attachment-dir "/Downloads"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t)

;;; Mail directory shortcuts
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"               . ?i)
           ("/[Gmail].Sent Mail"   . ?s)
           ("/[Gmail].Trash"       . ?t)
           ("/[Gmail].All Mail"    . ?a)))

;;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))
  ;;end mu4e
                                        ; end Org Capture
  (setq bibtex-dialect 'biblatex)
  (setq shell-default-shell 'ansi-term)
  (setq evil-move-cursor-back t)
  ;; (projectile-register-project-type 'npm '("package.json")
  ;;                                   :compile "npm build"
  ;;                                   :test "npm test"
  ;;                                   :run "npm start"
  ;;                                   :test-suffix ".test")
  (defun node-run-tests()
    "Run test"
    (interactive)
    (call-process "tmux" nil nil nil "send-keys" "-t" "potential-glossary:tests" "npm test\n"))

  (defun reload-browser ()
    "Reload the browser by using xdotool
            Do this by sending M-3 , sleep 0.5 seconds and then send <F5>"
    (interactive)
    ;; (call-process "xdotool" nil nil nil "key" "Super_L+3")
    (call-process "awesome-client" nil nil nil "require('awful').screen.focused().tags[3]:view_only()")
    (call-process "sleep" nil nil nil "0.7")
    (call-process "xdotool" nil nil nil "key" "F5"))

  (defun node-restart-server()
    "Restart server that is running inside tmux"
    (interactive)
    (call-process "tmux" nil nil nil "send-keys" "-t" "potential-glossary:server" "C-c")
    (call-process "sleep" nil nil nil "0.1")
    (call-process "tmux" nil nil nil "send-keys" "-t" "potential-glossary:server" " node app.js" "enter"))

  (defun node-reload-server-and-browser ()
    (interactive)
    (node-restart-server)
    (reload-browser))

  (spacemacs/set-leader-keys "ob" 'reload-browser)
  (spacemacs/set-leader-keys "os" 'node-restart-server)
  (spacemacs/set-leader-keys "or" 'node-reload-server-and-browser)
  (spacemacs/set-leader-keys "ot" 'node-run-tests)
  (push '(javascript-docs
          :name "Javascript")
        search-engine-alist)
  (defengine javascript-docs
    "http://devdocs.io/#q=javascript %s"
    :docstring "Search devdocs with javascript tag")

  (push '(rust-docs
          :name "Rust Docs")
        search-engine-alist)
  (defengine rust-docs
    "https://doc.rust-lang.org/std/?search= %s"
    :docstring "Search Rust documentation")

  
  (global-company-mode)
  (setq-default TeX-master nil)
  (spacemacs/set-leader-keys "tt" 'hl-todo-mode)
      
  (require 'org-drill))
;;; packages.el ends here
