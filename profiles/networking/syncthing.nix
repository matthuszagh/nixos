{ config
, lib
, pkgs
, ...
}:

{
  services.syncthing = {
    enable = true;
    declarative = {
      folders = {
        "src" = {
          path = "/home/matt/src";
          devices = [ "ryzen3950" "oryp4" ];
          ignorePerms = false;
        };
        "doc" = {
          path = "/home/matt/doc";
          devices = [ "ryzen3950" "oryp4" ];
          ignorePerms = false;
        };
        "gnupg" = {
          path = "/home/matt/.gnupg";
          devices = [ "ryzen3950" "oryp4" ];
          ignorePerms = false;
        };
        "ssh" = {
          path = "/home/matt/.ssh";
          devices = [ "ryzen3950" "oryp4" ];
          ignorePerms = false;
        };
        "emacs-src" = {
          path = "/home/matt/.config/emacs/straight/repos";
          devices = [ "ryzen3950" "oryp4" ];
          ignorePerms = false;
        };
      };

      devices = {
        ryzen3950 = {
          id = "ARE2A7I-MZ5DSK7-KDAH5WB-L4455LJ-JSJUNTW-XBOQKBF-KKJ3I5E-OP2LHAC";
          introducer = true;
        };
        oryp4 = {
          id = "LB4D4GR-KDB2QRT-5UOX5SW-CLLZNEJ-ARIQUOG-KXTAGHZ-OUTP5YS-F5O35A7";
          introducer = false;
        };
      };
    };

    user = "matt";
    group = "users";
    openDefaultPorts = true;
  };
}
