#!/usr/bin/env bash

echo "updating MacOS"
softwareupdate -l

echo "updating brew ..."
brew update && brew upgrade

echo "updating cs"
cs update

echo "updating sdkman"
sdk selfupdate && sdk upgrade

echo "updating dotfiles ..."
echo

echo "updating oh-my-zsh ..."
git -C oh-my-zsh fetch --all
EDITOR=vim git -C oh-my-zsh merge upstream/master
echo

echo "updating powerlevel10k ..."
git -C powerlevel10k pull
echo

echo "updating spacemacs ..."
git -C spacemacs fetch --all
EDITOR=vim git -C spacemacs merge upstream/develop
echo

echo "pushing submodules to origin ..."
git -C oh-my-zsh push
git -C spacemacs push
echo

echo "pushing dotfiles to origin ..."
git add .
gEDITOR=vim git commit -m "Update dotfiles and submodules"
git push

echo "current state:"
git status

echo
echo "done :-)"
