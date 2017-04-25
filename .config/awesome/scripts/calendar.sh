#!/bin/bash
export TERM=xterm-256color
cal -3 --color=always | sed 's/.\[//g' | sed '/^\s*$/d'
