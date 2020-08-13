self: super:

{
  # soundtouch = (super.soundtouch.overrideAttrs (oldAttrs: {
  #   makeFlags = "-fexceptions";
  # }));
  soundtouch = super.soundtouch;
}
