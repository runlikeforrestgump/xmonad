#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

#[[ -f $HOME/.config/X11/Xresources ]] && xrdb -merge $HOME/.config/X11/Xresources
#[[ -f $HOME/.config/X11/Xmodmap ]] && xmodmap $HOME/.config/X11/Xmodmap
#[[ -d $HOME/.fonts ]] && xset +fp $HOME/.fonts && xset fp rehash

urxvtd -q -o -f
feh --bg-scale $HOME/.wallpapers/Current
unclutter -idle 1 &

exec xmonad
