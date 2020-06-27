self: super:

let
  preserve-source = ''
    mkdir -p $out/src
    tar -xvf $src --directory $out/src
  '';
  custompkgs = import <custompkgs> {};
in
{
  octave = (super.octave.override {
    hdf5 = super.hdf5;
    ghostscript = super.ghostscript;
    # using gnuplot allows saving plots without displaying them. The
    # default fltk backend does not support this.
    gnuplot = super.gnuplot;
    # qt = super.qt5Full;
    # python = super.python3;
  }).overrideAttrs (attrs: {
    buildInputs = if attrs ? buildInputs
                    then attrs.buildInputs ++ [ super.gnutar custompkgs.gl2ps super.libGL ]
                    else [ super.gnutar custompkgs.gl2ps super.libGL ];
    postFixup = if attrs ? postFixup
                then attrs.postFixup + preserve-source
                else preserve-source;
  });
}
