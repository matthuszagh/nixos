{ pkgs
, lib
, ...
}:

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
  builtins.foldl' (init: file: init // {
    "${vpn_str file}" = {
      config = builtins.readFile ./pia-config.json;
      autoStart = false;
      up = "echo nameserver $nameserver | ${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
      down = "${pkgs.openresolv}/sbin/resolvconf -d $dev";
    };
  }) {} (
    builtins.attrNames (builtins.readDir "${pkgs.pia-config}/config")
  );
}
