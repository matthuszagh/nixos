{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    ispell
    languagetool
  ];
}
