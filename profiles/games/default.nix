{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    dolphinEmu
  ];
}
