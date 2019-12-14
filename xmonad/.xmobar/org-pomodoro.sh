#!/usr/bin/env bash

sed -e 's/^"//' -e 's/"$//' <<<"$(emacsclient -e '(my/org-pomodoro-text-time)')"
