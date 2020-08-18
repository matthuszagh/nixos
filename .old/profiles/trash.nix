{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    trash-cli
  ];
}
