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
        };
  }
