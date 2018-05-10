#!/bin/bash

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi
if [ -f ~/.bash_profile ]; then
    . ~/.bash_profile
fi

# git completion
if [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
    . /usr/local/etc/bash_completion.d/git-completion.bash
fi

export VISUAL=vim
export EDITOR=$VISUAL

# Shortcuts
alias ll='ls -al'
alias tarx="echo \*\*\* Tar GZip Extracting \*\*\*;tar -xzvf"
alias tarz="echo \*\*\* Tar GZip Compressing \*\*\*;tar -czvf"

# FTFY
alias vmi='vim'
alias vi='vim'
alias em='emacs'

# Keep file system changes verbose
alias rm='rm -v'
alias mv='mv -v'
alias cp='cp -v'

### bash tweaking
# Avoid duplicates
#export HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
#shopt -s histappend

# After each command, append to the history file and reread it
#export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
