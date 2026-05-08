#!/usr/bin/env bash

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if_confirmed() {
	read -r -p "$1 [Y/n] " answer || true
	case "${answer:-Y}" in
        [Yy] | [Yy][Ee][Ss] | "") return 0 ;;
        *)                        return 1 ;;
	esac
}

case "$(uname -s)" in
  Darwin)
    hm_target="darwin-dotfiles"
    if_confirmed "Do you want to install nix?"                                &&  sh <(curl -L https://nixos.org/nix/install)
    ;;
  Linux)
    hm_target="arch-dotfiles"
    if_confirmed "Do you want to install nix?"                                &&  yay -S nix
    if_confirmed "Do you want to enable the nix daemon service?"              &&  sudo systemctl enable --now nix-daemon.service
    ;;
  *)
    echo "Unsupported OS: $(uname -s)"
    exit 1
    ;;
esac

if_confirmed "Do you want to link config to ~/.config/nix?"               &&  ln -sfn "$repo_root/config" "$HOME/.config/nix"
if_confirmed "Do you want to verify flakes with 'nix flake show'?"        &&  cd "$repo_root" && nix flake show
if_confirmed "Do you want to build the Home Manager config?"              &&  cd "$repo_root" && nix run github:nix-community/home-manager/release-25.11 -- build --flake ".#$hm_target"
if_confirmed "Do you want to switch to the Home Manager config?"          &&  cd "$repo_root" && nix run github:nix-community/home-manager/release-25.11 -- switch -b backup --flake ".#$hm_target"
if [[ -d "$HOME/.emacs.doom" ]]; then
  echo "Doom Emacs already installed at ~/.emacs.doom, skipping."
else
  if_confirmed "Do you want to install Doom Emacs?" && git clone git@github.com:gbastkowski/doomemacs.git "$HOME/.emacs.doom" && "$HOME/.emacs.doom/bin/doom" install
fi
