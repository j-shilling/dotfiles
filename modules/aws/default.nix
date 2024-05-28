{ pkgs, ... }:

{
  programs.awscli = {
    enable = true;
    credentials = {
      "default" = {
        "credential_process" = "${pkgs.pass}/bin/pass show FunctorFactory/aws";
      };
      "seelthedeal" = {
        "credential_process" = "${pkgs.pass}/bin/pass show FunctorFactory/SeelTheDeal/aws";
      };
    };
    settings = {
      "default" = {
        region = "us-east-1";
        output = "json";
      };
      "seelthedeal" = {
        region = "us-east-1";
        output = "json";
      };
    };
  };
}
