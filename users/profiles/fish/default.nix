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
      rm = "${pkgs.trash-cli}/bin/trash";
    };
    interactiveShellInit = readFile ./config.fish;
  };
}
