final: prev:

{
  # kernel
  linux-gpib-kernel = prev.callPackage ./linux-gpib {
    kernel = prev.pkgs.linux_latest;
  };

  # general
  pia-config = prev.callPackage ./networking/vpn/pia { };
  clang_multi_9 = prev.wrapClangMulti prev.clang_9;
  gnucap = prev.callPackage ./applications/science/electronics/gnucap { };
  # pt1230 = prev.callPackage ./tools/printers/pt1230 { };
  kibom = prev.callPackage ./applications/science/electronics/kibom {
    inherit (prev.python3Packages)
      buildPythonApplication XlsxWriter;
  };
  brainworkshop = prev.callPackage ./games/brainworkshop {
    inherit (prev.python3Packages)
      buildPythonApplication pyglet;
  };
  ebase = prev.callPackage ./applications/science/electronics/ebase {
    inherit (prev.python3Packages)
      buildPythonApplication pyyaml
      psycopg2 pyusb kibom;
  };
  hdl-checker = prev.callPackage ./development/tools/hdl-checker {
    inherit (prev.python3Packages)
      buildPythonApplication
      fetchPypi
      prettytable
      argcomplete
      python-language-server
      requests
      tabulate
      bottle
      waitress;
  };

  openems = prev.callPackage ./applications/science/electronics/openems { };

  librarian = prev.callPackage ./applications/misc/librarian { };

  # python
  python3 = prev.python3.override {
    packageOverrides = pfinal: pprev: {
      circlib = pprev.callPackage ./development/python-modules/circlib { };
      pyems = pprev.callPackage ./development/python-modules/pyems { };
    };
  };
  python3Packages = final.python3.pkgs;

  # texlive
  circuitikz = {
    pkgs = [
      (prev.callPackage ./tools/typsetting/tex/packages/circuitikz_run.nix { })
    ];
  };
  qcircuit-tikz = {
    pkgs = [
      (prev.callPackage ./tools/typsetting/tex/packages/qcircuitz_run.nix { })
    ];
  };
  dvisvgm = {
    pkgs = [
      (prev.callPackage ./tools/typsetting/tex/packages/dvisvgm_bin.nix {
        texlive-bin = prev.texlive.bin.core;
      })
    ];
  };
  grffile = {
    pkgs = [
      (prev.callPackage ./tools/typsetting/tex/packages/grffile_run.nix { })
    ];
  };
  latexmk = {
    pkgs = [
      (prev.callPackage ./tools/typsetting/tex/packages/latexmk_bin.nix { })
    ];
  };
}
