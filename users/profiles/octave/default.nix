{ pkgs
, ...
}:

{
  home.file.".octaverc".text = ''
    # Don't save variables on unexpected exit.
    crash_dumps_octave_core(false)

    # Make OpenEMS available for use.
    addpath('${pkgs.openems}/share/openEMS/matlab');
    addpath('${pkgs.csxcad}/share/CSXCAD/matlab');
    addpath('${pkgs.hyp2mat}/share/hyp2mat/matlab');

    # Setup the OpenEMS interface to keep it in date.
    # setup;

    # Load installed packages.
    pkg load control
    pkg load signal
  '';
}
