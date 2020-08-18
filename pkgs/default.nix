self: super:

{
  pia-config = self.callPackage ./networking/vpn/pia { };
  clang_multi_9 = self.wrapClangMulti self.clang_9;
  gnucap = self.callPackage ./applications/science/electronics/gnucap { };
  # pt1230 = self.callPackage ./tools/printers/pt1230 { };
  kibom = self.callPackage ./applications/science/electronics/kibom {
    inherit (self.python3Packages)
      buildPythonApplication XlsxWriter;
  };
  brainworkshop = self.callPackage ./games/brainworkshop {
    inherit (self.python3Packages)
      buildPythonApplication pyglet;
  };
  ebase = self.callPackage ./applications/science/electronics/ebase {
    inherit (self.python3Packages)
      buildPythonApplication pyyaml
      psycopg2 pyusb kibom;
  };

  # python
  python3 = super.python3.override {
    packageOverrides = pself: psuper: {
      hdl-checker = pself.callPackage ./development/python-modules/hdl-checker { };
      skidl = pself.callPackage ./development/python-modules/skidl {
        inherit (self) kicad;
      };
      circlib = pself.callPackage ./development/python-modules/circlib { };
      pyems = pself.callPackage ./development/python-modules/pyems { };
    };
  };
  python3Packages = self.python3.pkgs;

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
