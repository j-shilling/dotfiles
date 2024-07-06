{ pkgs, ... }:

{
  programs.awscli = {
    enable = true;
    credentials = {
      "default" = {
        "credential_process" = "${pkgs.pass}/bin/pass show FunctorFactory/aws";
      };
      "seeltest" = {
        "credential_process" = "${pkgs.pass}/bin/pass show FunctorFactory/SeelTheDeal/aws-seeltest";
      };
      "seeldeals" = {
        "credential_process" = "${pkgs.pass}/bin/pass show FunctorFactory/SeelTheDeal/aws";
      };
    };
    settings = {
      "default" = {
        region = "us-east-1";
        output = "json";
      };
      "seeltest" = {
        region = "us-east-1";
        output = "json";
      };
      "seeldeals" = {
        region = "us-east-1";
        output = "json";
      };
    };
  };
}
