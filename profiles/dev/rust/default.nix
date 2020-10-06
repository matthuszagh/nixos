{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    rustc
    cargo
    rustup
    rls
    rustfmt
    racer
  ];
}
