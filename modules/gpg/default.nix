{ me, ... }:

{
  programs.gpg = {
    enable = true;
    settings = {
      use-agent = true;
      with-subkey-fingerprint = true;
      charset = "utf-8";
    };
  };

  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    enableSshSupport = true;
    enableZshIntegration = true;
    sshKeys = me.sshKeys;
    extraConfig =
''
pinentry-program "/mnt/c/Users/shill/scoop/apps/gpg4win/current/Gpg4win/bin/pinentry.exe"
'';
  };
}
