{ pkgs, ... }:

{
  home.packages = with pkgs; [
    slack
    signal-desktop
    discord
  ];
}
