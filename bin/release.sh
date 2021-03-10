#!/bin/sh

echo "Checking out master and pulling latest version"
git checkout master
git pull origin

echo "Current branches:"
echo "$(git branch -l)"
echo

read -p "Which branch do you want to merge? (Leave empty for no merge)" branch
case $version in
    "" ) echo "No branch selected. Creating new version without a merge commit."
         break;;

    * )  git merge --no-ff $branch
         break;
esac


while true
do
    read -p "Which version should be assigned? (latest: $(git tag | sort -V | head -n 1)) " version
    case $version in
        v[0-9]\.[0-9]\.[0-9]* ) break;;

        * )                     echo "Please enter a tag name like v<major>.<minor>.<micro>."
    esac
done

read -p "Please give release $version a catchy note: " message
git tag -a -m $message $version

while true
do
    read -p "Do you want to push the new version now? " push
    case $push in
        [yY]* ) echo "git push --follow-tags"
                break;;

        [nN]* ) echo "Didn't push the new release. Please remember to push with tags (git push --tags)."
                exit;;

        * )     echo "Please enter Y or N.";;
    esac
done
