{ ... }:

{
  programs.readline = {
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

}
