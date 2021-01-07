{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    # rustc
    # cargo
    rustup
    cargo-make
    itm-tools
    glibc
    rls
    rustfmt
    racer
  ];
}
