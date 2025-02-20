#!/usr/bin/env sh

UNAME=$(uname -a)

case "$UNAME" in
  *Android*)  echo termux ;;
  *arch*)     echo arch ;;
  *darwin*)   echo darwin ;;
esac
