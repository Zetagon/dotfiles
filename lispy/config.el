(add-hook 'clojure-mode-hook 'lispy-mode 'show-paren-mode)
(add-hook 'lispy-mode-hook #'lispyville-mode)

(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(operators
     (additional-movement normal visual motion))))
