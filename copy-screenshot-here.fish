#!/usr/bin/env fish
# Copies a screenshot from ~/screenshots/temp.png to ./images/(insert date here).png
# and puts the emacs link into the clipboard
set screenshotFile (pwd)/images/(date '+%Y-%m-%d_%X').png
mv ~/screenshots/temp.png  $screenshotFile
echo "[[file:"$screenshotFile"]]" | xclip -i -selection clipboard
