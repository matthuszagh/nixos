final: prev:

{
  csxcad = (prev.csxcad.override { }).overrideAttrs (oldAttrs: {
    src = prev.fetchFromGitHub {
      owner = "thliebig";
      repo = "CSXCAD";
      rev = "ea97d3992fa5c4f9a3b5c2c5639449504ccf7b90";
      sha256 = "sha256-O4+kZW2ol62GoIY0B4IlUOOP4awZM3n7+/+x8tB0nVg=";
    };
  });
}
