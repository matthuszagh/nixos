{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    clamav
  ];

  environment.etc."freshclam.conf".source = ./freshclam.conf;
}
