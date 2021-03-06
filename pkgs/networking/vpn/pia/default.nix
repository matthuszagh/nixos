{ lib
, stdenv
, fetchurl
, pkgs
, ...
}:

stdenv.mkDerivation {
  name = "pia-config";

  buildInputs = with pkgs; [
    unzip
    libuuid
  ];

  src = fetchurl {
    url = "https://www.privateinternetaccess.com/openvpn/openvpn.zip";
    sha256 = "sha256-vDhCd4Ku3JDLZbMizW+ddK9OmIzFs8iE5DI27XpeRJE=";
  };

  unpackPhase = ''
    unzip $src
  '';

  installPhase = ''
    mkdir -p "$out/uuids"
    ls *.ovpn | while read FILE; do
      uuidgen --md5 -n @url -N "$FILE" > "$out/uuids/$FILE"
    done

    mkdir -p "$out/config"
    mv *.ovpn "$out/config"

    mkdir -p "$out/certs"
    mv *.crt *.pem "$out/certs"
  '';

  fixupPhase = ''
    sed -i "s|crl.rsa.2048.pem|$out/certs/\0|g" "$out"/config/*.ovpn
    sed -i "s|ca.rsa.2048.crt|$out/certs/\0|g" "$out"/config/*.ovpn

    sed -i "s|auth-user-pass|auth-user-pass ${../../../../secrets/pia-login.conf}|g" "$out"/config/*.ovpn
  '';
}
