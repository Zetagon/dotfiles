;;; ~/.doom.d/pomodoro-pipe.el -*- lexical-binding: t; -*-

(defun my/org-pomodoro-pipe-init ()
  "Initialize the pipe at /tmp/pomodoro-pipe."
  (call-process "mkfifo" nil nil nil "/tmp/pomodoro-pipe")
  (my/org-pomodoro-pipe-clean))

(defun my/org-pomodoro-show-time ()
  "Return status info about org-pomodoro and if org-pomodoro is not running, try to print info about org-clock.
    If either org-pomodoro or org-clock aren't active, return \"No Active Task \" "
  (interactive)
  (cond ((equal :none org-pomodoro-state)
         (if (org-clock-is-active)
             (format "%s"
                     (substring-no-properties org-clock-heading)
                     "No Active task")))
        ((equal :pomodoro org-pomodoro-state)
         (format "%s" (substring-no-properties org-clock-heading)))
        ((equal :short-break org-pomodoro-state) "Short Break")
        ((equal :long-break org-pomodoro-state)  "Long Break")))

(defun my/org-pomodoro-pipe-update ()
  "Update the pipe with which task is currently clocked in"
  (append-to-file (concat (my/org-pomodoro-show-time) "\n") nil "/tmp/pomodoro-pipe"))

(defun my/org-pomodoro-pipe-clean ()
  "Put a newline in the pipe so that xmobar displays nothing"
  (append-to-file "\n" nil "/tmp/pomodoro-pipe"))

(defun my/org-pomodoro-pipe-set-hooks ()
  "Set the relevant hooks so that the pipe is automatically updated"
  (add-hook 'org-clock-in-hook #'my/org-pomodoro-pipe-update)
  (add-hook 'org-clock-out-hook #'my/org-pomodoro-clock-out-h))

(defun my/org-pomodoro-clock-out-h ()
  (if (and org-clock-current-task
           (string= "Transition" (substring-no-properties org-clock-current-task)))
      (my/org-pomodoro-pipe-clean)
    (my/clock-in-specific "Transition")))
