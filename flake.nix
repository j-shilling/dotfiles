{
  description = "Jake's Home Manager Config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url  = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
        nixpkgs
      , home-manager
      , emacs-overlay
      , ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        emacs-overlay.overlay
      ];
    };
    in
      (import ./homes/default.nix { inherit pkgs home-manager; });
}
