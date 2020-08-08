#!/usr/bin/env bash
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
# ~/.config/polybar/launch.sh

# Terminate already running bar instances
killall -q polybar

# Launch bar1
polybar bar1

echo "Bars launched..."
