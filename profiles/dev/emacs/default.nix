{ pkgs
, ...
}:

let
  emacsEnv = (pkgs.emacsPackagesFor (pkgs.emacsNative)).emacsWithPackages
  (epkgs: (with epkgs.elpaPackages; [
  ]) ++ (with epkgs.melpaPackages; [
    vterm
    lsp-python-ms  # microsoft python language server
  ]) ++ (with epkgs.orgPackages; [
  ]) ++ (with epkgs; [
    pdf-tools
  ]));
in
{
  environment.systemPackages = with pkgs; [
    emacsEnv
  ];
}
