#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

git pull origin master;

usage() {
    echo "Usage: $0 [-f|--force] [-n|--dry-run]";
    exit 1;
}

doIt() {
	  rsync --exclude ".git/" \
          --exclude ".gitmodules" \
          --exclude ".DS_Store" \
          --exclude "*.sh" \
		      --exclude "README.md" \
          --exclude "LICENSE-MIT.txt" \
          -avh --no-perms . ~;
	source ~/.bash_profile;
}

while getopts "fn" o; do
    case "${o}" in
        f)
            f=true
            ;;
        n)
            n=true
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ "${f}" ]; then
	doIt;
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1;
	echo "";
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		doIt;
	fi;
fi;
unset doIt;
