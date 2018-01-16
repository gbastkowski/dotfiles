#!/bin/bash

ln -sf $(pwd)/home/.spacemacs ~
ln -sf $(pwd)/spacemacs       ~/.emacs.d
ln -sf $(pwd)/sbt             ~/.sbt
ln -sf $(pwd)/oh-my-zsh       ~/.oh-my-zsh

git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k
