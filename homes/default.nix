{ pkgs, home-manager, ... }:
let
  profiles = import ./profiles.nix;

  mkHomeModule = { me, ... }:
    let
      profileModules = (builtins.map
        (mod: (import mod { inherit me pkgs; })
        )
        me.modules);
      baseModules = [
        {
          home = {
            packages = me.packages pkgs;
            username = me.username;
            homeDirectory = "/home/${me.username}";
            stateVersion = "24.05";

            sessionVariables = {
              EDITOR = "emacsclient -a '' -c";
            };
          };
          programs.home-manager.enable = true;
        }
      ];
      modules = baseModules ++ profileModules;
    in
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs modules;
      };

  homeConfigurations = (builtins.mapAttrs
    (profileName: me: mkHomeModule { inherit me; }
    )
    profiles);
in
{
  inherit homeConfigurations;
}
