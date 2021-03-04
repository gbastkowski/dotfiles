#!/bin/sh

LATEST_VERSION=$(git tag | sort -V | head -n 1)

echo "Current branches:"
echo "$(git branch -l)"
echo
echo "Latest version:"
echo "$LATEST_VERSION"

read -p "Which branch do you want to merge? " branch

# git merge --no-ff $branch


read -p "Which version should be assigned? (latest: $LATEST_VERSION) " version

# git tag -a $version

while true
do
    read -p "Do you want to push the new version now? " push
    # (2) handle the input we were given
    case $push in
        [yY]* ) echo "git push --tags"
                break;;

        [nN]* ) exit;;

        * )     echo "Please enter Y or N.";;
    esac
done
