{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    rustc
    cargo
    rls
    rustfmt
    racer
  ];
}
