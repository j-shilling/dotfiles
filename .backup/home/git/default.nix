_:

{
  programs.git = {
    enable = true;
    userEmail = "shilling.jake@gmail.com";
    userName = "Jake Shilling";
    delta.enable = true;
    signing = {
      key = "47BBC0300141D21B";
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
