{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    recoll
  ];
}
