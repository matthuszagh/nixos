final: prev:

{
  libftdi1 = (prev.libftdi1.overrideAttrs (old: {
    postInstall = old.postInstall + ''
      mkdir -p $out/bin
      cp examples/eeprom $out/bin/
    '';
  }));
}
