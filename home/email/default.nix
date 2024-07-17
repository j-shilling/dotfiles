{ pkgs, ... }:

{
  accounts.email = {
    accounts = {
      personal = {
        address = "shilling.jake@gmail.com";
        flavor = "gmail.com";
        gpg.key = "47BBC0300141D21B";
        gpg.signByDefault = true;
        mbsync.enable = true;
        mbsync.create = "both";
        mbsync.expunge = "both";
        mu.enable = true;
        primary = true;
        realName = "Jake Shilling";
        passwordCommand = "pass mail/shilling.jake@gmail.com";
      };
    };
  };

  programs.mbsync.enable = true;
  services.mbsync.enable = false;
  programs.mu.enable = true;

  home.packages = with pkgs; [
    isync
    msmtp
    mu
    emacsPackages.mu4e
    pass
  ];
}
