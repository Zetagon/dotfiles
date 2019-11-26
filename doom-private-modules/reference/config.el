;;; config.elReferences and notes -*- lexical-binding: t; -*-

(def-package! org-ref
  :config
  (setq org-ref-notes-directory "~/Dropbox/org/references/notes"
        org-ref-bibliography-notes "~/Dropbox/org/references/articles.org"
        org-ref-default-bibliography '("~/Dropbox/org/references/Zotero_articles.bib")
        org-ref-pdf-directory "~/Dropbox/org/references/pdfs/"
        org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f")
        org-ref-default-ref-type "cref"
        org-ref-default-citation-link "autocite")
  (map!
   :leader
   :n "nb" #'org-ref-open-bibtex-notes))

(def-package! helm-bibtex
  :config
  (setq helm-bibtex-bibliography "~/Dropbox/org/references/Zotero_articles.bib"
        reftex-default-bibliography '("~/Dropbox/org/references/Zotero_articles.bib")
        helm-bibtex-library-path "~/Dropbox/org/references/pdfs"
        bibtex-completion-notes-path "~/Dropbox/org/references/notes")
  (map!
   :leader
   :n "nh" #'helm-bibtex
   :n ))

(def-package! helm-org-rifle
  :config
  (setq helm-org-rifle-show-path t)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))

  (map!
   :leader
   :n "nr" (λ! (let ((helm-split-window-inside-p nil))
                 (helm-org-rifle-directories "~/Dropbox/org/references/notes")))))
(def-package! pdf-tools
  :config
  (map!
   :map pdf-view-mode-map
   :n "i" #'interleave-add-note
   :n "n" #'pdf-view-next-page-command
   :n "p" #'pdf-view-previous-page-command))
(def-package! interleave
  :config
  (map!
   :map interleave-mode-map
   :leader
   :n "nn" #'interleave-mode))

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))
(defun print-radio-toc ()
  (interactive)
  (let ((elements (re-seq "<<<.*>>>" (buffer-substring-no-properties 1 (buffer-end 1)))))
   (apply 'concat
          (map 'list (lambda (x) (insert (format "%s\n" (substring x 3 -3))))
               elements))))
(after! org
  )
(provide 'config)
;;; config.el ends here
