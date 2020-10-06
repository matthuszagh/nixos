final: prev:

{
  kicad-unstable = (prev.kicad-unstable.override {
    symbol-path = "/home/matt/src/kicad-symbols";
    footprint-path = "/home/matt/src/kicad-footprints";
    model3d-path = "/home/matt/src/kicad-packages3D";
    srcs = {
      "kicad-unstable" = {
        kicadVersion = {
          version = "2020-10-08";
          src = {
            rev = "fd22fe8e374ce71d57e9f683ba996651aa69fa4e";
            sha256 = "sha256-F8qugru/jU3DgZSpQXQhRGNFSk0ybFRkpyWb7HAGBdc=";
          };
        };
        libVersion = {
          version = "2020-08-22";
          libSources = {
            i18n.rev = "cbbb1efd940094bf0c3168280698b2b059a8c509";
            i18n.sha256 = "1q4jakn6m8smnr2mg7jgb520nrb6fag9mdvlcpx3smp3qbxka818";
            symbols.rev = "9ca6a5348cdeb88e699582d4ed051ff7303b44d3";
            symbols.sha256 = "13w6pb34rhz96rnar25z7kiscy6q1fm8l39hq1bpb8g9yn86ssz4";
            templates.rev = "ae16953b81055855bcede4a33305413599d86a15";
            templates.sha256 = "1pkv90p3liy3bj4nklxsvpzh9m56p0k5ldr22armvgqfaqaadx9v";
            footprints.rev = "f94c2d5d619d16033f69a555b449f59604d97865";
            footprints.sha256 = "1g71sk77jvqaf9xvgq6dkyvd9pij2lb4n0bn0dqnwddhwam935db";
            packages3d.rev = "f699b0e3c13fe75618086913e39279c85da14cc7";
            packages3d.sha256 = "0m5rb5axa946v729z35ga84in76y4zpk32qzi0hwqx957zy72hs9";
          };
        };
      };
    };
  });
}
