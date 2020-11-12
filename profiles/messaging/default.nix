{ pkgs
, ...
}:

{
  imports = [ ];

  environment.systemPackages = with pkgs; [
    element-desktop
  ];
}
