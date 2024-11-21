{ config, ... }:
{
  programs.fd.enable = true;

  programs.fzf = {
    enable = true;
    enableBashIntegration = config.programs.bash.enable;
    enableZshIntegration = config.programs.zsh.enable;
  };

  programs.ripgrep.enable = true;
}
