;;; private/haskell/config.el -*- lexical-binding: t; -*-

(def-package! intero
  :config
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (setq-local outline-regexp "-- [*\f]+")
               (outline-minor-mode)
               (intero-mode))))

(def-package! haskell-mode)
(add-hook 'intero-mode-hook (λ!
                             (setq company-idle-delay 0.5)
                             (haskell-indentation-mode -1)
                             (haskell-indent-mode 1))); haskell-indentation-mode fucks with evil-mode


(map! :after intero
      :map intero-mode-map
      :localleader
      :n "fd" #'intero-goto-definition
      :n "fr" #'intero-uses-at
      :nv "sS" #'intero-repl-eval-region
      :n "ss" #'intero-repl-load
      :n "ht" #'intero-type-at
      :n "hs" #'intero-apply-suggestions)
(map!                                  
 :map haskell-mode-map
 :localleader
 :nv "t" #'haskell-mode-show-type-at
 :nv "rt" #'my-hspec-run-closest-test
 :nv "c" (λ! (compile compile-command)))

(after! projectile
  (setq projectile-project-types (assq-delete-all 'haskell-stack projectile-project-types))
  (projectile-register-project-type 'haskell-stack '("stack.yaml")
                                    :compile "stack build"
                                    :run "stack run"
                                    :test "stack test"
                                    :src-dir "src"
                                    :test-suffix "Spec"
                                    :test-dir "test"))

(defun my-hspec-run-closest-test ()
  (interactive)

  ;;                    v  match spaces
  ;;                             v match the function describe
  ;;                                          v match any spaces
  ;;                                               v match anything inside quotes
  (save-excursion
    (goto-char (line-end-position))
    (re-search-backward "\\(^ *\\)\\(describe\\) *\"\\( *.+\\)\""))
  (let ((test-name (match-string 3)))
    (if test-name
      (progn
        (setq compile-command (concat "stack test --test-arguments='-m\"" test-name "\"'"))
        ;; (setenv "MYHSPECARG" test-name)
        ;; (kill-compilation)
        (compile compile-command)
        )
      (message "No tests found!"))))

