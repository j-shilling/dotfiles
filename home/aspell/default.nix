{ pkgs, ... }:
let
  aspell =     pkgs.aspellWithDicts (d: [
    d.en
  ]);
  in
  {
    home.packages = [
      aspell
    ];
  }
