self: super:

let
  version = "master";
  src = /home/matt/src/pypi2nix;
  click = super.fetchurl {
    url = "https://pypi.python.org/packages/95/d9/c3336b6b5711c3ab9d1d3a80f1a3e2afeb9d8c02a7166462f6cc96570897/click-6.7.tar.gz";
    sha256 = "02qkfpykbq35id8glfgwc38yc430427yd05z1wc5cnld8zgicmgi";
  };

  requests = super.fetchurl {
    url = "https://pypi.python.org/packages/16/09/37b69de7c924d318e51ece1c4ceb679bf93be9d05973bb30c35babd596e2/requests-2.13.0.tar.gz";
    sha256 = "1s0wg4any4dsv5l3hqjxqk2zgb7pdbqhy9rhc8kh3aigfq4ws8jp";
  };
in
{
  pypi2nix = (super.pypi2nix.overrideAttrs (attrs: {
    name = "pypi2nix-${version}";
    srcs = [
      src
      click
      requests
    ];

    sourceRoot = src;

    postUnpack = ''
      mkdir -p $out/pkgs

      mv click-*/click                    $out/pkgs/click
      mv requests-*/requests              $out/pkgs/

      if [ -z "$IN_NIX_SHELL" ]; then
        if [ -e git-export ]; then
          mv git-export/src/pypi2nix      $out/pkgs/pypi2nix
        else
          mv src/pypi2nix          $out/pkgs/pypi2nix
        fi
      fi
    '';
  }));
}
