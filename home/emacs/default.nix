{ config, pkgs, ... }:

{
  fonts.fontconfig = {
    enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: with epkgs; [
      ace-window
      apheleia
      consult
      corfu
      diff-hl
      diminish
      envrc
      eshell-syntax-highlighting
      eshell-vterm
      gcmh
      haskell-mode
      helpful
      ibuffer-vc
      ligature
      magit
      marginalia
      modus-themes
      nerd-icons
      nerd-icons-completion
      nerd-icons-corfu
      nerd-icons-dired
      nerd-icons-ibuffer
      nix-mode
      nix-ts-mode
      no-littering
      orderless
      poetry
      smartparens
      terraform-doc
      terraform-mode
      treesit-grammars.with-all-grammars
      tsc
      vertico
      vlf
      vterm
      yasnippet
    ];
  };

  xdg.configFile.emacs = {
    enable = true;
    recursive = true;
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/emacs/emacs";
  };

  home.packages = with pkgs; [
    nil
    fira-sans
    fira-mono
    fira-code
  ];
}
