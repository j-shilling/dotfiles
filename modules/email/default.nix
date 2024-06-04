{ me, ... }:

{
  accounts.email = with me; {
    accounts = {
      personal = {
        address = email;
        flavor = "gmail.com";
        gpg.key = gpgKey;
        gpg.signByDefault = true;
        mbsync.enable = true;
        mbsync.create = "both";
        mbsync.expunge = "both";
        mu.enable = true;
        primary = true;
        realName = me.name;
        passwordCommand = "pass mail/shilling.jake@gmail.com";
      };
    };
  };

  programs.mbsync.enable = true;
  services.mbsync.enable = false;
  programs.mu.enable = true;
}
