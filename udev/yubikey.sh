#!/bin/sh

xtty="$(cat /sys/class/tty/tty0/active)"
xuser="$(who | grep "${xtty}" | head -n 1 | cut -d' ' -f1)"
xdisplay="$(ps -A -o user= -o cmd= | grep Xwayland | grep -v grep | grep ${xuser} | awk '{print $3}')"
xwayland_pid="$(ps -u ${xuser} -o pid= -o cmd= | grep "Xwayland ${xdisplay}" | awk '{print $1}')"
xwayland_environ="/proc/${xwayland_pid}/environ"

environment="$(cat ${xwayland_environ} | tr '\0' ' ')  \
	DISPLAY=${xdisplay}                            \
	SSH_ASKPASS=/usr/lib/seahorse/ssh-askpass"

title=YubiKey

send_notification () {
	read -d '' message
	/bin/su "$xuser" -c "${environment} /usr/bin/notify-send -t 2000 \"YubiKey\" \"${message}\""
}

add () {
	send_notification <<-EOF
		Device attached.
		Adding private key from YubiKey.
		You'll be asked to enter the PIN.
	EOF
	/bin/su "$xuser" -c "${environment} /usr/bin/ssh-add -s /usr/lib/libykcs11.so"
}

remove () {
	send_notification <<-EOF
		Device removed.
		Removing cached YubiKey SSH keys.
	EOF
	/bin/su "$xuser" -c "${environment} /usr/bin/ssh-add -e /usr/lib/libykcs11.so"
}

main () {
	if [ "$1" = "add" ]
	then
		add
	fi

	if [ "$1" = "remove" ]
	then
		remove
	fi
}

main "$@"

