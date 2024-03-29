# XDG Base directory
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Add user bin to PATH
export PATH="$HOME/.local/bin:$PATH"

# Set zsh paths
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export ZSH="$XDG_DATA_HOME/oh-my-zsh"
export ZSH_CACHE="$XDG_CACHE_HOME/zsh"
export HISTFILE="$ZSH_CACHE/history"
export ZSH_COMPDUMP="$ZSH_CACHE/zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

# Set gnupg paths
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

# Set other cluttering variables
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"

# Add texlive to PATHs
export PATH=/mnt/hdd/texlive/bin/x86_64-linux:$PATH
export MANPATH=/mnt/hdd/texlive/texmf-dist/doc/man:$MANPATH
export INFOPATH=/mnt/hdd/texlive/texmf-dist/doc/info:$INFOPATH

# Set Taskwarrior paths
export TASKRC="$XDG_CONFIG_HOME/task/taskrc"
export TASKDATA="$XDG_DATA_HOME/task"

# Set Android studio path
export ANDROID_HOME="$XDG_DATA_HOME/android"

# Set java path
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java

# Set X11 files path
export ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors"

# Set platformio path
export PLATFORMIO_CORE_DIR="$XDG_DATA_HOME/platformio"

# Set ICEauthority path
export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority

# Set java path
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java

# Set cargo and rustup paths
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
[ -f "$CARGO_HOME/env" ] && source "$CARGO_HOME/env"

# Set isync directory
export MBSYNCRC="$XDG_CONFIG_HOME"/isync/mbsyncrc

# Set mysql history file
export MYSQL_HISTFILE="$XDG_DATA_HOME"/mysql_history
