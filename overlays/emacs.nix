final: prev:

{
  emacsNative = (prev.pkgs.emacsGcc.override {
    withGTK3 = true;
    withXwidgets = true;
    withX = true;
    srcRepo = true;
    withCsrc = true;
  }).overrideAttrs (old: {
    CFLAGS = "-O3 -march=native -momit-leaf-frame-pointer";
  });
}
