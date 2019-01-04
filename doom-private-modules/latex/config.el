;;; private/latex/config.el -*- lexical-binding: t; -*-
(after! latex
  (add-hook 'LaTeX-mode-hook
            (λ!
             (message "latex-mode-hook!")
             (add-to-list 'TeX-command-list
                          '("LatexMk" "latexmk"
                            TeX-run-TeX nil t :help "Run latexmk" t) t)
             (setq TeX-command-default "latex Make")
             (setq helm-bibtex-bibliography  "~/Dropbox/org/references/Zotero_articles.bib"))))
;; TODO add fill paragraph
;;

(defun sync-external-bib-file ()
  (interactive)
  (let ((default-directory (file-name-directory buffer-file-name)))
       (shell-command-to-string (concat "biber "
                                        (file-name-sans-extension (buffer-name))
                                        " --output-format bibtex"))))
 (defun pdf-reference/init-interleave()
   (use-package interleave))

(map! :after latex
      :localleader
      :map TeX-mode-map
      :n "m" #'TeX-insert-macro
      :n "b" #'latex/build
      :n "s" #'LaTeX-section
      :n "rs" #'sync-external-bib-file
      :n "rh" #'helm-bibtex
      (:desc "fonts" :prefix "x"
          :desc "bold" :vn "b"  #'latex/font-bold
          :desc "code" :vn "c"  #'latex/font-code
          :desc "emph" :vn "e"  #'latex/font-emphasis
          :desc "italic" :vn "i"  #'latex/font-italic
          :desc "clear" :vn "r"  #'latex/font-clear
          :desc "oblique" :vn "o"  #'latex/font-oblique
          :desc "small caps" :vn "fc" #'latex/font-small-caps
          :desc "sans serif" :vn "ff" #'latex/font-sans-serif
          :desc "serif" :vn "fr" #'latex/font-serif))
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX"))
(defun latex/build ()
  ;; (interactive)
  ;; (progn
  ;;   (let ((TeX-save-query nil))
  ;;     (TeX-save-document (TeX-master-file)))
  ;;   (TeX-command latex-build-command 'TeX-master-file -1)))
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk" "latexmk -pdf -f" master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error t)
      (minibuffer-message "latexmk done"))))

  ;; (map! :map rust-mode-map
  ;;       :localleader
  ;;       :prefix "b"
  ;;       :n "b" (λ! (compile "cargo build --color always"))
  ;;       :n "c" (λ! (compile "cargo check --color always"))
  ;;       :n "r" (λ! (compile "cargo run --color always"))
  ;;       :n "t" (λ! (compile "cargo test --color always"))))
