{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> {
    inherit system;
  };
  pyspice-pkgs = import (builtins.fetchTarball {
    name = "matthuszagh-nixpkgs-pyspice-2020-05-23";
    url = "https://github.com/matthuszagh/nixpkgs/archive/8516c750b95b0b97538445df84f264ba67792834.tar.gz";
    sha256 = "0fhg3878dj0ql7nzsmqawnf1q0xr22zabch977ffvi647ly3ga64";
  }) {};

  callPackage = pkgs.lib.callPackageWith (pkgs // self);
  self = rec {
    # TODO remove all of these when openems PR accepted
    vtk = callPackage ./pkgs/vtk {
      inherit (pkgs.xlibs) libX11 xorgproto libXt;
      inherit (pkgs.darwin) libobjc;
      inherit (pkgs.darwin.apple_sdk.libs) xpc;
      inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices DiskArbitration
                                                 IOKit CFNetwork Security ApplicationServices
                                                 CoreText IOSurface ImageIO OpenGL GLUT;
    };
    vtkWithQt4 = vtk.override { qtLib = pkgs.qt4; };
    octave = callPackage ./pkgs/octave {
      libX11 = pkgs.xlibs.libX11;
    };
    fparser = callPackage ./pkgs/fparser { };
    csxcad = callPackage ./pkgs/csxcad { };
    qcsxcad = callPackage ./pkgs/qcsxcad { };
    appcsxcad = callPackage ./pkgs/appcsxcad { };
    openems = callPackage ./pkgs/openems { withMPI = false; };
    openems-doc = callPackage ./pkgs/openems-doc {
      python = pkgs.python3.withPackages (p: with p; [
        sphinx
        sphinx_rtd_theme
        numpydoc
      ]);
      openems = openems;
      csxcad = csxcad;
    };
    openmpi = callPackage ./pkgs/openmpi { };
    hyp2mat = callPackage ./pkgs/hyp2mat { };
    gl2ps = callPackage ./pkgs/gl2ps { };
    cgal = callPackage ./pkgs/cgal { };
    python-openems = callPackage ./pkgs/python-openems {
      buildPythonPackage = pkgs.python3Packages.buildPythonPackage;
      python = pkgs.python3;
      pythonPackages = pkgs.python3Packages;
      python-csxcad = python-csxcad;
      cython = pkgs.python3Packages.cython;
      openems = (openems.override { withMPI = false; });
      csxcad = csxcad;
    };
    python-csxcad = callPackage ./pkgs/python-csxcad {
      buildPythonPackage = pkgs.python3Packages.buildPythonPackage;
      python = pkgs.python3;
      pythonPackages = pkgs.python3Packages;
      cython = pkgs.python3Packages.cython;
      openems = openems;
      csxcad = csxcad;
    };

    kicad = callPackage ./pkgs/kicad { };

    # primerun = callPackage ./pkgs/primerun { };
    brainworkshop = callPackage ./pkgs/brainworkshop { };
    kibom = callPackage ./pkgs/kibom { };
    avbin = callPackage ./pkgs/avbin { };
    libav = callPackage ./pkgs/libav { };
    pt1230 = callPackage ./pkgs/pt1230 { };

    # TODO remove when vivado PR accepted
    vivado-2017-2 = callPackage ./pkgs/vivado/2017 {
      inherit (pkgs.xlibs) libSM libICE libX11 libXrender libxcb libXext libXtst libXi;
    };

    vivado-2019-1 = callPackage ./pkgs/vivado/2019 {
      inherit (pkgs.xlibs) libSM libICE libX11 libXrender libxcb libXext libXtst libXi;
    };

    pia-config = callPackage ./pkgs/pia { };

    nixSrc = callPackage <nixpkgs>/pkgs/tools/package-management/nixops/generic.nix (rec {
      version = "git";
      src = /home/matt/src/nix;
    });

    gnucap = callPackage ./pkgs/gnucap { };

    # clang 9 multilib
    clang_multi_9 = pkgs.wrapClangMulti pkgs.clang_9;

    # texlive overrides
    latexmk.pkgs = [(callPackage ./pkgs/latexmk { })];
    dvisvgm.pkgs = [(callPackage ./pkgs/dvisvgm {
      texlive-bin = pkgs.texlive.bin.core;
    })];
    circuitikz.pkgs = [(callPackage ./pkgs/circuitikz { })];
    grffile.pkgs = [(callPackage ./pkgs/grffile { })];
    # luatex.pkgs = [(callPackage ./pkgs/luatex { })];

    # emacs packages
    org-recoll = callPackage ./pkgs/org-recoll { };
    layers = callPackage ./pkgs/layers { };
    org-db = callPackage ./pkgs/org-db { };
    org-mode = callPackage ./pkgs/org-mode { };
    justify-kp = callPackage ./pkgs/justify-kp { };
    helm = callPackage ./pkgs/helm { };
    helm-org = callPackage ./pkgs/helm-org { };
    helm-ls-git = callPackage ./pkgs/helm-ls-git { };
    org-ql = callPackage ./pkgs/org-ql { };
    yasnippet = callPackage ./pkgs/yasnippet { };

    # python packages
    cocotb = callPackage ./pkgs/cocotb { };
    lieer = callPackage ./pkgs/lieer { };
    # TODO remove when skidl PR accepted
    skidl = callPackage ./pkgs/skidl {
      kinparse = kinparse;
      pyspice = pyspice-pkgs.python3Packages.pyspice;
      pykicad = pykicad;
      # pillow = pkgs.python3Packages.pillow;
      # pytest = pkgs.python3Packages.pytest;
      # tox = pkgs.python3Packages.tox;
      python = pkgs.python3;
    };
    kinparse = callPackage ./pkgs/kinparse { };
    pykicad = callPackage ./pkgs/pykicad { };
    pyspice = pyspice-pkgs.python3Packages.pyspice;
    # pyspice = callPackage ./pkgs/pyspice { };
    # hdl_checker = callPackage ./pkgs/hdl_checker { };
    pylibgen = callPackage ./pkgs/pylibgen { };
    pylibftdi = callPackage ./pkgs/pylibftdi {
      inherit (pkgs) libusb1;
    };

    # wrapped packages
    tree-sitter-wrapped = callPackage ./wrapped/tree-sitter { };

    # personal packages
    ebase = callPackage ./pkgs/ebase { kibom = kibom; };
    libdigital = pkgs.callPackage ./pkgs/libdigital { cocotb = cocotb; };
    circlib = pkgs.callPackage ./pkgs/circlib {
      skidl = skidl;
    };
    python3.pkgs.pyems = pkgs.callPackage ./pkgs/pyems {
      python-csxcad = python-csxcad;
      python-openems = python-openems;
    };
  };
in self
