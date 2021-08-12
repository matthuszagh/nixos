{ pkgs
, ...
}:

{
  programs.bash = {
    shellAliases = {
      ls = "${pkgs.coreutils}/bin/ls --color=auto";
      ll = "${pkgs.coreutils}/bin/ls -Alh";
    };
    enableCompletion = true;
  };

  environment.systemPackages = with pkgs; [
    nodePackages.bash-language-server
    shellcheck
  ];
}
