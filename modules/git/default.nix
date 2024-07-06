{ me, ... }:

{
  programs.git = with me; {
    enable = true;
    userEmail = email;
    userName = name;
    delta.enable = true;
    signing = {
      key = gpgKey;
      signByDefault = true;
    };
    extraConfig = {
      core = {
        autocrlf = "input";
        whitespace = "-trailing-space,-space-before-tab,-cr-at-eol";
      };
      init.defaultBranch = "main";
      merge = {
        log = true;
        renormalize = true;
      };
      pull.rebase = true;
      fetch.prune = true;
      github.user = "j-shilling";
      gitlab.user = "shilling.jake";
    };
  };
}
