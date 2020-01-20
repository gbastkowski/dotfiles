#!/bin/sh
echo "Reloading SSH agent and private key from Yubikey. You'll be asked for the PIN"
gnome-keyring-daemon --replace && ssh-add -s /usr/lib/pkcs11/opensc-pkcs11.so
echo "SSH keys loaded:"
ssh-add -l
