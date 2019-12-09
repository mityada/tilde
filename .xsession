#!/bin/sh

export PATH=$HOME/bin:$PATH

xsetroot -cursor_name left_ptr

light-locker &
xterm &
firefox &
xflux -l 59.7 -g 30.3 -k 3000
#stalonetray &
telegram-desktop &
