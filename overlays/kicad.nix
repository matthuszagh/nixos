final: prev:

{
  kicad-unstable = (prev.kicad-unstable.override { }).overrideAttrs (old: {
    version = "2020-10-04";

    src = prev.fetchFromGitLab {
      group = "kicad";
      owner = "code";
      repo = "kicad";
      rev = "8dd7777108620bf939a5c512ca06f792e34fa137";
      sha256 = "sha256-BbYEEGy/CfJQC7hkK/JV/HvESZlLBbh4MAL9I975RXI=";
      # sha256 = "0000000000000000000000000000000000000000000000000000";
    };
  });
}
