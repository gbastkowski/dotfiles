#!/usr/bin/env sh

UNAME=$(uname -a)

case "$UNAME" in
  *Android*)  echo termux ;;
  *arch*)     echo arch ;;
  *Darwin*)   echo darwin ;;
esac
