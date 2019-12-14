;;; configs.el org-pomodoro -*- lexical-binding: t; -*-

(use-package! org-pomodoro
  :config
  (add-hook 'org-pomodoro-finished-hook (λ! (message-box "The basic 25 minutes on this dreadful task are not up; it's a shame to see you leave."))))

(provide 'configs)
;;; configs.el ends here
