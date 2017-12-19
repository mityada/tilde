#!/bin/sh

export PATH=$PATH:$HOME/bin

xsetroot -cursor_name left_ptr

light-locker &
xterm &
firefox &
psi-plus &
xflux -l 59.7 -g 30.3 -k 3000
stalonetray &
