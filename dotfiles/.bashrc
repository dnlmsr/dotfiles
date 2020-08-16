#    _   ___  ___          _
#   | |  |  \/  |         (_)
#  / __) | .  . | ___  ___ _ ___
#  \__ \ | |\/| |/ _ \/ __| / __|
#  (   / | |  | | (_) \__ \ \__ \
#   |_|  \_|  |_/\___/|___/_|___/
#
# Daniele Moser
# dnlmsr0@gmail.com
#
# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

#PS1 configuration
PS1='\[\e[01;33m\]┌─[\[\e[01;32m\]\u\[\e[00m\]:\[\e[1;34m\]\w\[\e[01;33m\]]\n└─╼ \[\e[0m\]'

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Aliases

# ls aliases
alias ls='ls -a --color=auto'
alias la='ls -alh --color=auto'

# Networking tools
alias ping='ping -c 5'
alias ipw='curl ipinfo.io/ip'

# sudo commands
alias pacman='sudo pacman'
alias poweroff='sudo poweroff'
alias reboot='sudo reboot'

alias untar='tar -zxvf'
alias mkdir='mkdir -pv'

# termial aliases
alias c='clear'
alias q='exit'
alias h='history'

# git aliases
alias gs='git status '
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias gco='git checkout '
