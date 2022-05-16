# XDG Base directory
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

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
export LESSHISTFILE="$XDG_CACHE_HOME/lesshst"

# Add texlive to PATHs
export PATH=/mnt/hdd/texlive/2021/bin/x86_64-linux:$PATH
export MANPATH=/mnt/hdd/texlive/2021/texmf-dist/doc/man:$MANPATH
export INFOPATH=/mnt/hdd/texlive/2021/texmf-dist/doc/info:$INFOPATH
