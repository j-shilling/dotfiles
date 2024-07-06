{
  jake = {
    username = "jake";
    name = "Jake Shilling";
    email = "shilling.jake@gmail.com";
    gpgKey = "47BBC0300141D21B";
    sshKeys = [
      "E556265A9520AFE6C5BEC85C47B1ADB883CCBC91"
    ];
    modules = [
      ../modules/shell
      ../modules/gpg
      ../modules/email
      ../modules/git
      ../modules/aws
      ../modules/python
      ../modules/javascript
    ];
    packages = pkgs: with pkgs; [
      emacs-pgtk
      nil
      languagetool
    ];
  };
}
