;;; config.el -*- lexical-binding: t; -*-
(def-package! exwm
  :config
  (require 'exwm-config)
;;;; Move between windows
  (setq exwm-input-global-keys `(([?\s-l] . evil-window-right )
                                 ([?\s-h] . evil-window-left )
                                 ([?\s-k] . evil-window-up )
                                 ([?\s-j] . evil-window-down )
                                 ([?\s-b] . helm-buffers-list)))
;;;; Helm bindings
  (map! :after helm
        :leader
        :n "." #'helm-find-files
        :n "B" #'helm-buffer-list)
;;;; Multple monitors
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "VGA-0" 1 "HDMI-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  (exwm-randr-enable)
;;;; Firefox windows as buffers with better names
  (defun pnh-trim-non-ff ()
    (delete-if-not (apply-partially 'string-match "- Mozilla Firefox$")
                   ido-temp-list))
  (add-hook 'exwm-manage-finish-hook
            (defun pnh-exwm-manage-hook ()
              (when (string-match "Firefox" exwm-class-name)
                (exwm-workspace-move-window 3)
                (exwm-layout-hide-mode-line)
                (setq ido-make-buffer-list-hook 'pnh-trim-non-ff))
              (when (string-match "Chromium" exwm-class-name)
                (exwm-workspace-move-window 1)
                (exwm-layout-hide-mode-line))))

  (add-hook 'exwm-update-title-hook
            (defun pnh-exwm-title-hook ()
              (when (string-match "Firefox" exwm-class-name)
                (exwm-workspace-rename-buffer exwm-title))))

  (setq browse-url-firefox-arguments '("-new-window"))

  (setq exwm-input-simulation-keys
      '(([\C-t] . [\C-n])))
;;;; Initialize
  (exwm-config-default))


;;; config.el ends here
