#!/bin/bash

STOW_PACKAGES=(byobu emacs)

for package in $STOW_PACKAGES
do
    stow -t $HOME -vR $package
done

#
# ZSH setup
#
# mkdir -p $(pwd)/oh-my-zsh/custom/themes
# ln -sf $(pwd)/powerlevel10k/   $(pwd)/oh-my-zsh/custom/themes/
# ln -sf $(pwd)/oh-my-zsh        ~/.oh-my-zsh

# ln -sf $(pwd)/home/.spacemacs  ~
# ln -sf $(pwd)/home/.ideavimrc  ~
# ln -sf $(pwd)/spacemacs        ~/.emacs.d
# ln -sf $(pwd)/spacemacs-layers ~/.spacemacs-layers
# ln -sf $(pwd)/sbt              ~/.sbt
# ln -sf $(pwd)/oh-my-zsh        ~/.oh-my-zsh
