#!/usr/bin/env sh

case "$(uname -a)" in
  *Android*)  echo termux ;;
  *arch*)     echo arch ;;
  *Darwin*)   echo darwin ;;
esac
