{ pkgs }:

{
  environment.systemPackages = with pkgs; [
    node-packages.bash-language-server
    shellcheck
  ]
}
