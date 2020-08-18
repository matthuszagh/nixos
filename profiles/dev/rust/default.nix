{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    rustc
    rls
    rustfmt
    cargo
  ];
}
