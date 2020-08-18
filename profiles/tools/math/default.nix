{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    # TODO fix
    # sageWithDoc
  ];
}
