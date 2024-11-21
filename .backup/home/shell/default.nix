_: {
    programs.jq.enable = true;

    programs.lsd = {
      enable = true;
      enableAliases = true;
    };

    programs.pandoc.enable = true;

    programs.password-store.enable = true;

    programs.man.enable = true;
}
