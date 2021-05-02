{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    ledger
  ];
}
