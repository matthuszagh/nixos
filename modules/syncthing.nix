{ config, lib, pkgs, ... }:

{
  services.syncthing = {
    enable = true;
    declarative = {
      folders = {
        "src" = {
          path = "/home/matt/src";
          devices = [ "ryzen3950" "oryp4" ];
        };
        "doc" = {
          path = "/home/matt/doc";
          devices = [ "ryzen3950" "oryp4" ];
        };
        "gnupg" = {
          path = "/home/matt/.gnupg";
          devices = [ "ryzen3950" "oryp4" ];
        };
        "ssh" = {
          path = "/home/matt/.ssh";
          devices = [ "ryzen3950" "oryp4" ];
        };
      };

      devices = {
        ryzen3950 = {
          id = "ARE2A7I-MZ5DSK7-KDAH5WB-L4455LJ-JSJUNTW-XBOQKBF-KKJ3I5E-OP2LHAC";
          introducer = true;
        };
        oryp4 = {
          id = "YQIDZ2A-IQO2SYY-ESQWJ5I-WOLAWEM-VCL6ET3-7TTEUQD-CZYF3KO-VGSFRAO";
          introducer = false;
        };
      };
    };

    user = "matt";
    group = "users";
    openDefaultPorts = true;
  };
}
