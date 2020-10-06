{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    djvulibre
    djvu2pdf
    djview
  ];
}
