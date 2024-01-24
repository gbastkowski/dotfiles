#!/usr/bin/env bash

echo "updating MacOS"
softwareupdate -l

echo "updating brew ..."
brew update && brew upgrade

echo "updating cs"
cs update

echo "updating sdkman"
sdk selfupdate && sdk upgrade

echo "updating pip stuff"
for i in $(pip list -o | awk 'NR > 2 {print $1}')
do
    pip install -U $i
done

echo "updating dotfiles ..."
echo

echo "updating oh-my-zsh ..."
git -C zsh/.oh-my-zsh fetch --all
EDITOR=vim git -C zsh/.oh-my-zsh merge upstream/master
echo

echo "updating powerlevel10k ..."
git -C powerlevel10k pull
echo

echo "updating zsh-vi-mode ..."
git -C zsh-vi-mode pull
echo

echo "updating spacemacs ..."
git -C emacs/.emacs.d fetch --all
EDITOR=vim git -C emacs/.emacs.d checkout develop
EDITOR=vim git -C emacs/.emacs.d merge upstream/develop
EDITOR=vim git -C emacs/.emacs.d push
EDITOR=vim git -C emacs/.emacs.d checkout gunnar
EDITOR=vim git -C emacs/.emacs.d merge develop
EDITOR=vim git -C emacs/.emacs.d push
echo

echo "updating reveal.js ..."
git -C reveal.js fetch --all
EDITOR=vim git -C reveal.js merge upstream/master
echo

echo "pushing submodules to origin ..."
git -C zsh/.oh-my-zsh push
git -C emacs/.emacs.d push
echo

echo "pushing dotfiles to origin ..."
git add .
EDITOR=vim git commit -m "Update dotfiles and submodules"
git push

echo "current state:"
git status

echo
echo "done :-)"
