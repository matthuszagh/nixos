final: prev:

{
  csxcad = (prev.csxcad.override { }).overrideAttrs (oldAttrs: {
    version = "2022.05.19";

    src = prev.fetchFromGitHub {
      owner = "thliebig";
      repo = "CSXCAD";
      rev = "cd9decb4d9cebe3c8bf115e2c0ee73f730f22da1";
      sha256 = "sha256-NquEspnDg4pJc65M+uSFaW3MFe+fvwOZR7Wdu2FVBJg=";
    };
  });
}
