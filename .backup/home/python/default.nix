{ pkgs, ... }: {
  programs.pyenv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
  home.packages = with pkgs; [
    poetry
    (python312.withPackages (p: with p; [
      pip
    ]))
  ];
}
