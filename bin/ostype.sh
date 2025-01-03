#!/usr/bin/env sh

UNAME=$(uname -a)

case "$UNAME" in
  *Android*)
      echo termux
    ;;
esac
