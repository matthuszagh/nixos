{ pkgs
, lib
, ...
}:

let
  inherit (builtins) foldl' attrNames readFile readDir;
in
{
  environment.systemPackages = with pkgs; [
    openresolv
  ];

  # Configure all our servers
  # Use with `sudo systemctl start openvpn-us-east`
  services.openvpn.servers = let
    vpn_str = with lib.strings;
              file: removeSuffix ".ovpn" (toLower (replaceStrings [" "] ["-"] file));
  in
  foldl' (init: file: init // {
    "${vpn_str file}" = {
      config = readFile ./pia-config.json;
      autoStart = false;
      up = "echo nameserver $nameserver | ${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
      down = "${pkgs.openresolv}/sbin/resolvconf -d $dev";
    };
  }) {} (
    attrNames (readDir "${pkgs.pia-config}/config")
  );
}
