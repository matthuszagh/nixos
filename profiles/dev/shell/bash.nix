{ pkgs
, ...
}:

{
  programs.bash = {
    shellAliases = {
      ls = "${pkgs.coreutils}/bin/ls --color=auto";
      ll = "${pkgs.coreutils}/bin/ls -Alh";
      rm = "${pkgs.trash-cli}/bin/trash";
    };
    enableCompletion = true;
  };

  environment.systemPackages = with pkgs; [
    nodePackages.bash-language-server
    shellcheck
  ];
}
