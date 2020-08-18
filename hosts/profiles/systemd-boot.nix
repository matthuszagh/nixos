{
  boot.loader = {
   systemd-boot = {
     enable = true;
     consoleMode = "max";
     editor = true;
   };
   efi.canTouchEfiVariables = true;
  };
}
