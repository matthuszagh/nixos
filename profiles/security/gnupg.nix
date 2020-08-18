{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    pinentry
  ];

  programs.gnupg = {
    agent = {
      enable = true;
      pinentryFlavor = "tty";
    };
  };
}
