{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    k2pdfopt
  ];
}
