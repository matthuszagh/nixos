{ pkgs
, ...
}:

{
  hardware = {
    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  sound.enable = true;

  # environment.systemPackages = with pkgs; [
  #   pulseaudio
  # ];
}
