{
  description = "Home Manager MVP for Gunnar's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.11";
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

  outputs = { nixpkgs, home-manager, nix-darwin, ... }@inputs:
    let
      commonModules = [
        ./common.nix
        ./bin.nix
        ./byobu.nix
        ./direnv.nix
        ./gpg.nix
        ./idea.nix
        ./zsh.nix
        ./tmux.nix
        ./git.nix
        ./emacs.nix
        ./kitty.nix
        ./claude.nix
        ./plover.nix
        ./sbt
      ];

      mkHost = system: hostModules: home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};
        extraSpecialArgs = { inherit inputs; };
        modules = commonModules ++ hostModules;
      };
    in {
      homeConfigurations = {
        "ista-dotfiles"  = mkHost "aarch64-darwin" [ ./hammerspoon ./mvn ./ista.nix ];
        "akiko-dotfiles" = mkHost "x86_64-linux"   [ ./hypr ./akiko.nix ];
      };

      darwinConfigurations.deess1mac = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = { inherit inputs; };
        modules = [
          ./darwin
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit inputs; };
            home-manager.users."gunnar.bastkowski" = { ... }: {
              imports = commonModules ++ [ ./hammerspoon ./mvn ./ista.nix ];
            };
          }
        ];
      };
    };
}
