{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: with epkgs; [
      vterm
      helpful
      ace-window
      ibuffer-vc
      vertico
      corfu
      orderless
      consult
      marginalia
      yasnippet
      smartparens
      modus-themes
      nix-mode
      nix-ts-mode
      treesit-grammars.with-all-grammars
      tsc
      nerd-icons
      nerd-icons-dired
      nerd-icons-ibuffer
      nerd-icons-completion
      nerd-icons-corfu
    ];
  };

  xdg.configFile.emacs = {
    enable = true;
    recursive = true;
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/emacs/emacs";
  };

  home.packages = with pkgs; [
    nil
  ];
}
