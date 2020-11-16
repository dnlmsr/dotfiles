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

# PS1 configuration
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export PS1='\[\e[01;33m\]┌─[\[\e[32m\]\u\[\e[00m\]:\[\e[34m\]\w\[\e[01;33m\]] \[\e[01;31m\]$(parse_git_branch)\[\e[01;33m\]\n└─╼ \[\e[0m\]'

# bash autocompletion
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Emacs client
export EDITOR="emacsclient -t -a ''"      # $EDITOR use Emacs in terminal
export VISUAL="emacsclient -c -a 'emacs'"   # $VISUAL use Emacs in GUI mode

# shopt
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s histappend # do not overwrite history

### ARCHIVE EXTRACTION ###
# usage: ex <file>
ex ()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1   ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *.deb)       ar x $1      ;;
            *.tar.xz)    tar xf $1    ;;
            *.tar.zst)   unzstd $1    ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}


### ALIASES ###

# ls aliases
alias ls='exa -lah --color=always --group-directories-first'
alias lt='exa -aT --color=always --group-directories-first'

# networking tools
alias ping='ping -c 5'
alias ipw='curl ipinfo.io/ip'

# sudo commands
alias pacman='sudo pacman'
alias poweroff='sudo poweroff'
alias reboot='sudo reboot'

# file managing
alias mkdir='mkdir -pv'
alias rm='rm -i'
alias mv='mv -i'

# termial aliases
alias c='clear'
alias q='exit'
alias h='history'
alias wdil='history|grep '
alias tb='nc termbin.com 9999'
alias tbc='nc termbin.com 9999 | xclip -selection c'

# git aliases
alias gs='git status '
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias gco='git checkout '

# emacs clients
alias em="emacsclient -nw -a ''"
alias emacs="emacsclient -c -a 'emacs'"

# colored cat
alias cat="bat"
alias catp="bat --style=plain "
