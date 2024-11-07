# https://git.sr.ht/~bwolf/dotfiles/tree/master/item/hosts/homes.nix

{ inputs, pkgs, ... }:

let
  home = inputs.home;

  mkHomeConfig = { user, configPath }: home.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      (
        { ... }: {
          home = {
            username = user;
            homeDirectory = "/home/" + user;
            stateVersion = "24.05";
          };

          programs.home-manager.enable = true;
          xdg.enable = true;

          nix = {
            package = pkgs.nix;
            settings = {
              experimental-features = ["nix-command" "flakes"];
              substituters = ["https://cache.nixos.org" "https://nix-community.cachix.org"];
              trusted-public-keys = [
                "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                ];
            };
          };

          nixpkgs = {
            config = {
              allowUnfree = true;
            };
          };
        }
      )
      configPath
    ];
  };
  in
  {
    homeConfigurations =
      let
        user = "jake";
        in
        {
          "jake@wsl" = mkHomeConfig {
            inherit user;
            configPath = ./wsl/home.nix;
          };
          "jake@linux" = mkHomeConfig {
            inherit user;
            configPath = ./linux/home.nix;
          };
        };
  }
