{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    djvu-libre
  ];
}
