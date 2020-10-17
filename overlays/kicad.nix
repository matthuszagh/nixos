final: prev:

{
  kicad-unstable = (prev.kicad-unstable.override {
    srcs = {
      kicadVersion = "2020-10-08";
      kicad = prev.fetchFromGitLab {
        group = "kicad";
        owner = "code";
        repo = "kicad";
        rev = "fd22fe8e374ce71d57e9f683ba996651aa69fa4e";
        sha256 = "sha256-F8qugru/jU3DgZSpQXQhRGNFSk0ybFRkpyWb7HAGBdc=";
      };
    };
  });
}
