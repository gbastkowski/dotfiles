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
	# /usr/bin/runuser -u "$xuser" -- ${environment} notify-send -t 2000 "YubiKey" "${message}"
	echo /bin/su "$xuser" -c "${environment} notify-send -t 2000 \"YubiKey\" \"${message}\""
}

add () {
	send_notification <<-EOF
		Device attached.
		Adding private key from YubiKey.
		You'll be asked to enter the PIN.
	EOF

	# /bin/su "$xuser" -c "${environment} ssh-add -s /usr/lib/libykcs11.so"
}

remove () {
	send_notification <<-EOF
		Device removed.
		Removing cached YubiKey SSH keys.
	EOF
	# /bin/su "$xuser" -c "${environment} xdg-screensaver lock"
	# /bin/su "$xuser" -c "${environment} ssh-add -e /usr/lib/libykcs11.so"
}

main () {
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
}

main "$@"

