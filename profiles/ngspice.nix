{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    ngspice
  ];
}