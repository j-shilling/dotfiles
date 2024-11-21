{
  description = "Jake's Home Manager Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url  = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
  self
  , nixpkgs
  , home
  , emacs-overlay
  , ...
  }@inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        emacs-overlay.overlay
      ];
    };
    isWSL = (builtins.getEnv "WSL_DISTRO_NAME") != "";

    homes = pkgs.callPackage ./hosts/homes.nix {
      inherit self system pkgs inputs;
    };

    default_home_key = (if isWSL then "jake@wsl" else "jake@linux");

    default_home_value = builtins.getAttr default_home_key homes.homeConfigurations;

    in
    {
      homeConfigurations = homes.homeConfigurations // { "jake" = default_home_value; };
    };
}
