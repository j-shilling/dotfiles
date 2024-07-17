{ ... }:

{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    historyControl = [
      "erasedups"
    ];
    historyIgnore = [
      "ls"
      "exit"
      "history"
      "clear"
    ];
    shellOptions = [
      "histappend"
      "cmdhist"
      "checkwinsize"
      "autocd"
      "dirspell"
      "cdspell"
      "globstar"
      "nocaseglob"
    ];
  };
}
