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
    profileExtra = ''
      # Rust executables installed via Cargo
      PATH=$PATH:~/.cargo/bin
    '';
  };
}
