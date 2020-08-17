super: self: {
  # texlive
  circuitikz = { pkgs = [
    (self.callPackage ./tools/typsetting/tex/packages/circuitikz_run.nix { })
  ]; };
  dvisvgm = { pkgs = [
    (self.callPackage ./tools/typsetting/tex/packages/dvisvgm_bin.nix {
      texlive-bin = super.texlive.bin.core;
    })
  ]; };
  grffile = { pkgs = [
    (self.callPackage ./tools/typsetting/tex/packages/grffile_run.nix { })
  ]; };
  latexmk = { pkgs = [
    (self.callPackage ./tools/typsetting/tex/packages/latexmk_bin.nix { })
  ]; };
}
