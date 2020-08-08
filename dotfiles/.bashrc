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

case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

    ;;
  screen*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
    ;;
esac

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion
