{ config
, pkgs
, ...
}:

{
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    package = pkgs.mesa_drivers;
  };
}
