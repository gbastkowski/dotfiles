#!/bin/sh

add () {
	su gunnar -c "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus DISPLAY=:0 notify-send -t 2000 \"YubiKey Attached\" \"Adding private key from YubiKey. You'll be asked to enter the PIN.\""
	su gunnar -c "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus DISPLAY=:0 SSH_AUTH_SOCK=/run/user/1000/keyring/ssh SSH_ASKPASS=/usr/lib/seahorse/ssh-askpass ssh-add -s /usr/lib/libykcs11.so"
}

remove () {
	su gunnar -c "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus DISPLAY=:0 notify-send -t 2000 \"YubiKey Removed\" \"Removing cached YubiKey SSH keys.\""
	su gunnar -c "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus DISPLAY=:0 SSH_AUTH_SOCK=/run/user/1000/keyring/ssh ssh-add -e /usr/lib/libykcs11.so"
	su gunnar -c "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus DISPLAY=:0 xdg-screensaver lock"
}

if [[ "$1" = "add" ]]
then
       	grep -q available /tmp/yubikey || add
	echo available > /tmp/yubikey
fi

if [[ "$1" = "remove" ]]
then
	grep -q available /tmp/yubikey && remove
	rm /tmp/yubikey
fi
