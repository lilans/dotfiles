if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

setxkbmap -layout "us,ru" -option "grp:rctrl_toggle,grp:swapcaps"

xsetroot -cursor_name left_ptr &
exec xmonad
