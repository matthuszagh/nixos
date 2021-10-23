{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    compsize
    btrbk
  ];
}
