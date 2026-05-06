#!/usr/bin/env bash

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if_confirmed() {
	read -r -p "$1 [Y/n] " answer || true
	case "${answer:-Y}" in
        [Yy] | [Yy][Ee][Ss] | "") return 0 ;;
        *)                        return 1 ;;
	esac
}

if_confirmed "Do you want to install nix?"                                &&  sh <(curl -L https://nixos.org/nix/install)
if_confirmed "Do you want to link nix/config to ~/.config/nix?"           &&  ln -sfn "$repo_root/nix/config" "$HOME/.config/nix"
if_confirmed "Do you want to verify flakes with 'nix flake show ./nix'?"  &&  cd "$repo_root" && nix flake show ./nix
if_confirmed "Do you want to build the Home Manager config?"              &&  cd "$repo_root" && nix run github:nix-community/home-manager/release-25.11 -- build --flake ./nix#darwin-dotfiles
if_confirmed "Do you want to switch to the Home Manager config?"          &&  cd "$repo_root" && nix run github:nix-community/home-manager/release-25.11 -- switch -b backup --flake ./nix#darwin-dotfiles
