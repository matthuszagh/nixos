{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    youtube-dl
    shotcut # video editing (preferred)
    ffmpeg
  ];
}
