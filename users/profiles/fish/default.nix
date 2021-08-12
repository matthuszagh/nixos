{ pkgs
, ...
}:
let
  inherit (builtins) readFile;
in
{
  programs.fish = {
    enable = true;
    shellAliases = {
      ll = "${pkgs.coreutils}/bin/ls -Alh --color";
    };
    interactiveShellInit = readFile ./config.fish;
  };
}
