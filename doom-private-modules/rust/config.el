;;; private/rust/config.el -*- lexical-binding: t; -*-

(map!
      :after racer
      :localleader
      :map racer-mode-map
      :n "fd" #'racer-find-definition
      :n "fD" #'racer-find-definition-other-window
      :n "hd" #'racer-describe-tooltip
      :n "bd" (λ! (compile "cargo doc --color always")))
