{ pkgs, ... }: {
  home.packages = with pkgs; [
    haskellPackages.ghcup
  ];
}
