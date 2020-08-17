{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    texlab
  ];
}
