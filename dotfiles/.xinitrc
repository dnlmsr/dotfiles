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
# ~/.xinitrc

# Set keyboard layout
setxkbmap it

# Load Xresources
[[ -f ~/.Xresources ]] && xrdb ~/.Xresources

# Xorg keyboard rate
xset r rate 200 30

# Start udiskie daemon
udiskie -Nt &

# Start i3wm
exec i3
