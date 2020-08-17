{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    nodePackages.typescript-language-server
    nodePackages.typescript
  ];
}
