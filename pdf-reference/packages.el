;;; packages.el --- pdf-reference layer packages file for Spacemacs.
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
;; added to `pdf-reference-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `pdf-reference/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `pdf-reference/pre-init-PACKAGE' and/or
;;   `pdf-reference/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

 (defconst pdf-reference-packages
   '(helm-bibtex
     org-ref
     interleave)
   "The list of Lisp packages required by the pdf-reference layer.

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

 (defun pdf-reference/init-helm-bibtex()
   (use-package helm-bibtex
     :config
     (setq helm-bibtex-bibliography "~/Dropbox/org/references/Zotero_articles.bib"
           reftex-default-bibliography '("~/Dropbox/org/references/articles.bib")
           helm-bibtex-library-path "~/Dropbox/org/references/pdfs"
           helm-bibtex-notes-path "~/Dropbox/org/references/articles.org")
     )
 )
 (defun pdf-reference/init-org-ref()
   (use-package org-ref
     :config
     (progn
      (setq org-ref-notes-directory "~/Dropbox/org/references/notes"
           org-ref-bibliography-notes "~/Dropbox/org/references/articles.org"
           org-ref-default-bibliography '("~/Dropbox/org/references/articles.bib")
           org-ref-pdf-directory "~/Dropbox/org/references/pdfs/"))

     :init
     (progn
     (spacemacs/declare-prefix "ab" "Bibliography")
     (spacemacs/set-leader-keys "abh" 'helm-bibtex
       "abi" 'interleave
       "abn" 'org-ref-open-bibtex-notes
       "abd" 'doi-add-bibtex-entry
       "abc" 'crossref-add-bibtex-entry
       "abs" 'sync-external-bib-file
       )))
 )

(defun sync-external-bib-file ()
  (interactive)
  (let ((default-directory (file-name-directory buffer-file-name)))
       (shell-command-to-string (concat "biber "
                                        (file-name-sans-extension (buffer-name))
                                        " --output-format bibtex"))))
 (defun pdf-reference/init-interleave()
   (use-package interleave))
 ;;; packages.el ends here

