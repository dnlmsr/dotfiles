# XDG Base directory
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# Add user bin to PATH
export PATH="$HOME/.local/bin:$PATH"

# Set zsh config path
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Add texlive to PATHs
export PATH=/mnt/hdd/texlive/2021/bin/x86_64-linux:$PATH
export MANPATH=/mnt/hdd/texlive/2021/texmf-dist/doc/man:$PATH
export INFOPATH=/mnt/hdd/texlive/2021/texmf-dist/doc/info:$PATH
