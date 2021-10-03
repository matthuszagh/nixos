{ config
, lib
, pkgs
, ...
}:

{
  # TODO this is a system-wide service, but should really be
  # user-specific.

  # TODO how should I handle authinfo? It seems like some of this
  # should be shared while some of it shouldn't.

  # TODO syncthing currently fails on the initial sync because the
  # user 'matt' does not have permissions to create
  # /var/lib/syncthing. The solution to this is probably to set the
  # configuration directory within the user directory rather than a
  # outside it.
  services.syncthing = {
    enable = true;
    declarative = {
      folders = {
        "src" = {
          path = "/home/matt/src";
          devices = [ "ryzen3950" "oryp4" "st5" ];
          ignorePerms = false;
        };
        "doc" = {
          path = "/home/matt/doc";
          devices = [ "ryzen3950" "oryp4" "st5" ];
          ignorePerms = false;
        };
        # TODO I shouldn't synchronize gnupg or ssh. I should probably
        # just synchronize authorized keys, known hosts, etc.
        "gnupg" = {
          path = "/home/matt/.gnupg";
          devices = [ "ryzen3950" "oryp4" ];
          ignorePerms = false;
        };
        # TODO make this declarative (home-manager should be handling
        # the syncthing service anyway)
        #
        # the following should be in .stignore:
        #
        # id_rsa
        # id_rsa.pub
        # known_hosts
        # known_hosts.old
        "ssh" = {
          path = "/home/matt/.ssh";
          devices = [ "ryzen3950" "oryp4" "st5" ];
          ignorePerms = false;
        };
        # TODO make this declarative (home-manager should be handling
        # the syncthing service anyway)
        #
        # the following should be in .stignore:
        #
        # auto-save-list
        # %backup%~
        # .cask
        # .dap-breakpoints
        # .edbi-ds-history
        # eln-cache
        # elpa
        # elpy
        # etc
        # .extension
        # idlwave
        # straight/build
        # straight/build-cache.el
        # straight/modified
        # var
        "emacs" = {
          path = "/home/matt/.config/emacs";
          devices = [ "ryzen3950" "oryp4" "st5" ];
          ignorePerms = false;
        };
      };

      devices = {
        ryzen3950 = {
          id = "RGHYOS7-B2EPXFS-3XBTP5V-AUHUFKF-N3CUM3A-IOWPVO3-7UIGG2Y-4DFAUQS";
          introducer = true;
        };
        # TODO this will need to be updated
        oryp4 = {
          id = "LB4D4GR-KDB2QRT-5UOX5SW-CLLZNEJ-ARIQUOG-KXTAGHZ-OUTP5YS-F5O35A7";
          introducer = false;
        };
        st5 = {
          id = "YKTPQOZ-WSRAPJV-B6QWUMP-VMIXPT3-FLFAXDM-74LYPT5-PC454ZQ-JBIQ2AP";
          introducer = false;
        };
      };
    };

    user = "matt";
    group = "users";
    openDefaultPorts = true;
    configDir = "/home/matt/.config/syncthing";
    dataDir = "/home/matt/.config/syncthing";
  };
}
