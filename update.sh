#!/usr/bin/env bash

echo "updating dotfiles ..."
echo

echo "updating oh-my-zsh ..."
git -C oh-my-zsh fetch --all
git -C oh-my-zsh merge upstream/master
echo

echo "updating powerlevel9k ..."
git -C powerlevel9k pull
echo

echo "updating spacemacs ..."
git -C spacemacs fetch --all
git -C spacemacs merge upstream/develop
echo

echo "pushing submodules to origin ..."
git -C oh-my-zsh push
git -C spacemacs push
echo

echo "pushing dotfiles to origin ..."
git add .
git commit -m "Update dotfiles and submodules"
git push

echo "current state:"
git status

echo
echo "done :-)"
