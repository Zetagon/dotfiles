;;; config.el --- description -*- lexical-binding: t; -*-
;;;;
;; ;; ;; ;; ;; 
(def-package! lispy
  :config
  (add-to-list 'lispy-compat 'cider)
  (add-to-list 'evil-escape-excluded-major-modes 'clojure-mode)
  (add-to-list 'evil-escape-excluded-major-modes 'emacs-lisp-mode))
(add-hook! clojure-mode
  (lispy-mode 1))
(add-hook! cider-mode
  #'cider-company-enable-fuzzy-completion)
(add-hook! cider-repl-mode
  #'cider-company-enable-fuzzy-completion)
(add-hook! emacs-lisp-mode (lispy-mode 1))

;; (add-hook! clojure-mode
;;   (eval-after-load "lispy"
;;     `(progn
;;        (lispy-define-key lispy-mode-map "e" 'cider-eval-last-sexp)
;;        (lispy-define-key lispy-mode-map "E" 'cider-insert-last-sexp-in-repl))))
;; (add-hook! emacs-lisp-mode
;;   (eval-after-load "lispy"
;;     `(progn
;;        (lispy-define-key lispy-mode-map "e" 'lispy-eval)
;;        (lispy-define-key lispy-mode-map "E" 'lispy-eval-and-insert))))
;;; config.el ends here
