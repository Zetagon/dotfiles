;;; private/haskell/config.el -*- lexical-binding: t; -*-

(def-package! intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(def-package! haskell-mode)
(add-hook 'intero-mode-hook (λ!
                             (setq company-idle-delay 0.5)
                             (haskell-indentation-mode -1)
                             (haskell-indent-mode 1))); haskell-indentation-mode fucks with evil-mode

(map! :after intero
      :map intero-mode-map
      :localleader
      "fd" #'intero-goto-definition
      "fr" #'intero-uses-at
      "sS" #'intero-repl-eval-region
      "ss" #'intero-repl-load
      "ht" #'intero-type-at
      "hs" #'intero-apply-suggestions)

