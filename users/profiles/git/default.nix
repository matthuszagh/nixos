{ pkgs
, ...
}:

{
  programs.git = {
    enable = true;
    extraConfig = {
      core.editor = "vim";
      merge.renameLimit = "999999";
    };

    lfs.enable = true;
  };
}
