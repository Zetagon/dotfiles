#!/usr/bin/env bash

if [[ -z $(pgrep $1) ]]; then
    echo "$1 not running!";
else
    echo "";
fi
