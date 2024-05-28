{ lib, pkgs, ... }:

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
      work = {
        address = "jshilling@functorfactory.com";
        flavor = "gmail.com";
        realName = "Jake Shilling";
      };
    };
  };
  fonts.fontconfig = {
    enable = true;
  };
  manual.manpages.enable = true;
  programs = {
    awscli = {
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
    bash = {
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
    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    fd.enable = true;
    fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
    git = {
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
      };
    };
    gpg = {
      enable = true;
      settings = {
        use-agent = true;
        with-subkey-fingerprint = true;
        charset = "utf-8";
      };
    };
    jq.enable = true;
    lsd = {
      enable = true;
      enableAliases = true;
    };
    man.enable = true;
    mbsync.enable = true;
    mu.enable = true;
    pandoc.enable = true;
    password-store = {
      enable = true;
    };
    pyenv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
    readline = {
      enable = true;
      bindings = {
        "\e[A" = "history-search-backward";
        "\e[B" = "history-search-forward";
      };
      variables = {
        bell-style = "none";
        bind-tty-special-chars = true;
        blink-matching-paren = true;
        colored-completion-prefix = true;
        colored-stats = true;
        completion-display-width = 0;
        completion-ignore-case = true;
        completion-map-case = true;
        enable-bracketed-paste = true;
        expand-tilde = true;
        match-hidden-files = true;
        show-all-if-ambiguous = true;
        show-mode-in-prompt = false;
        mark-symlinked-directories = true;
        editing-mode = "emacs";
        keymap = "emacs";
        meta-flag = true;
        convert-meta = false;
        output-meta = true;
      };
    };
    ripgrep.enable = true;
    ssh = {
      enable = true;
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableBashIntegration = true;
      enableSshSupport = true;
      enableZshIntegration = true;
      sshKeys = [
        "E556265A9520AFE6C5BEC85C47B1ADB883CCBC91"
      ];
    };
  };

  home = {
    packages = with pkgs; [
      emacs-pgtk
      libvterm
      nil
    ];

    username = "jake";
    homeDirectory = "/home/jake";

    stateVersion = "24.11";
  };
}
