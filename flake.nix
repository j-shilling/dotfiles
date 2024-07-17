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

    homes = pkgs.callPackage ./hosts/homes.nix {
      inherit self system pkgs inputs;
    };

    in
    {
      # homeConfigurations = {
      #   jake = home-manager.lib.homeManagerConfiguration {

      #     extraSpecialArgs = { inherit inputs outputs; };
      #     modules = [
      #       ./home.nix
      #       ./modules/emacs
      #       ./modules/email
      #       ./modules/shell
      #       ./modules/git
      #       ./modules/gpg
      #       ./modules/aws
      #       ./modules/python
      #       ./modules/javascript
      #     ];
      #   };
      # };
      homeConfigurations = homes.homeConfigurations;
    };
}
