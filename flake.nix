{
  description = "Home Manager MVP for Gunnar's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    powerlevel10k = {
      url = "github:romkatv/powerlevel10k";
      flake = false;
    };
    zsh-vi-mode = {
      url = "github:jeffreytse/zsh-vi-mode";
      flake = false;
    };
    ista-dotfiles = {
      url = "git+ssh://git@github.com/gbastkowski/ista-dotfiles";
      flake = false;
    };
    chemacs2 = {
      url = "github:plexus/chemacs2";
      flake = false;
    };
  };

  outputs = { nixpkgs, home-manager, ... }@inputs: {
    homeConfigurations = {
      "darwin-dotfiles" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.aarch64-darwin;
        extraSpecialArgs = { inherit inputs; };
        modules = [
          ./home/common.nix
          ./home/bin.nix
          ./home/byobu.nix
          ./home/direnv.nix
          ./home/gpg.nix
          ./home/idea.nix
          ./home/zsh.nix
          ./home/tmux.nix
          ./home/git.nix
          ./home/emacs.nix
          ./home/kitty.nix
          ./home/claude.nix
          ./home/plover.nix
          ./home/sbt.nix
          ./home/darwin.nix
        ];
      };

      "arch-dotfiles" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [
          ./home/common.nix
          ./home/bin.nix
          ./home/byobu.nix
          ./home/direnv.nix
          ./home/gpg.nix
          ./home/idea.nix
          ./home/zsh.nix
          ./home/tmux.nix
          ./home/git.nix
          ./home/emacs.nix
          ./home/kitty.nix
          ./home/claude.nix
          ./home/plover.nix
          ./home/sbt.nix
          ./home/arch.nix
        ];
      };
    };
  };
}
