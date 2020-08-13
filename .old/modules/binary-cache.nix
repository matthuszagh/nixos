{ hostName
, config
, pkgs
, ...
}:

{
  services.nix-serve = {
    enable = true;
    secretKeyFile = "/home/matt/src/dotfiles/nixos/security/bcache-${hostName}-priv-key.pem";
  };

  services.nginx = {
    enable = true;
    enableReload = true;
    virtualHosts = {
      "binarycache.${hostName}.com" = {
        serverAliases = [ "binarycache" ];
        locations."/".extraConfig = ''
          proxy_pass http://localhost:${toString config.services.nix-serve.port};
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        '';
        # addSSL = true;  # enable HTTPS
      };
    };
  };
}
