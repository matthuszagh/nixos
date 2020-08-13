{ pkgs, ... }:

let custompkgs = import <custompkgs> {};
in
{
  home.file.".octaverc".text = ''
    # Don't save variables on unexpected exit.
    crash_dumps_octave_core(false)

    # Make OpenEMS available for use.
    addpath('${custompkgs.openems}/share/openEMS/matlab');
    addpath('${custompkgs.csxcad}/share/CSXCAD/matlab');
    addpath('${custompkgs.hyp2mat}/share/hyp2mat/matlab');

    # Setup the OpenEMS interface to keep it in date.
    # setup;

    # Load installed packages.
    pkg load control
    pkg load signal
  '';
}
