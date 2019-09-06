;;; config.el --- description -*- lexical-binding: t; -*-


(def-package! lsp-java
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(def-package! lsp-mode )
(def-package! lsp-ui
  :config
  (setq lsp-ui-doc-max-width 50)
  (setq lsp-ui-doc-max-height 25)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-prefer-flymake nil)
  (evil-snipe-mode))
(map!
 :map lsp-ui-mode-map
 :nvmi "<C-return>" #'lsp-ui-sideline-apply-code-actions
 :nm "s" #'lsp-ui-doc-show
 :nm "gd" #'lsp-ui-peek-find-definitions
 :n "c" (general-key-dispatch 'evil-change
          "r" #'my-lsp-format-region
          "n" #'lsp-rename)
 :n "n" '(menu-item
          ""
          nil
          :filter (lambda (&optional _)
                    (when (eq evil-this-operator 'change)
                      #'lsp-rename)))
 (:leader
   :nm "h." #'lsp-describe-thing-at-point)
 (:localleader
   :nm "fr" #'lsp-ui-peek-find-references)
 :map lsp-ui-peek-mode-map
 :nmi "C-p" #'lsp-ui-peek--select-prev
 :nmi "C-n" #'lsp-ui-peek--select-next)

;; (evil-define-minor-mode-key 'operator 'lsp-mode
;;   "r" '(menu-item
;;         ""
;;         nil
;;         :filter (lambda (&optional _)
;;                   (when (memq evil-this-operator '(evil-change))
;;                     #'my-lsp-format-region)))
;;   "n" '(menu-item
;;         ""
;;         nil
;;         :filter (lambda (&optional _)
;;                   (when (memq evil-this-operator '(evil-change))
;;                     #'lsp-rename))))
(def-package! company-lsp)
(def-package! lsp-java-treemacs
  :after (treemacs))
(evil-define-operator my-lsp-format-region (beg end)
  (lsp-format-region beg end))

;;; config.el ends here
