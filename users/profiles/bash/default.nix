{ pkgs
, ...
}:
let
  inherit (builtins) readFile;
in
{
  programs.bash = {
    enable = true;
    initExtra = readFile ./bashrc;
  };
}
