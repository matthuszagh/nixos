{ pkgs
, ...
}:
let
  emacsEnv = (pkgs.emacsPackagesFor (pkgs.emacsNative)).emacsWithPackages (epkgs:
    (with epkgs.melpaPackages; [
      vterm
      pdf-tools
    ]));
in
{
  environment.systemPackages = [
    emacsEnv
  ];

  # services.emacs = {
  #   enable = true;
  #   package = emacsEnv;
  #   defaultEditor = true;
  #   install = true;
  # };
}
