{ pkgs
, ...
}:

{
  home.file.".mbsyncrc".source = ./mbsyncrc;
}
