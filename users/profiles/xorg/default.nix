{ config
, ...
}:
let
  # TODO
  # useNvidia = builtins.any (v: v == true) (builtins.attrValues config.hardware.nvidia);
  useNvidia = false;
in
{
  home.file.".xinitrc".text = (if useNvidia then ''
    xrandr --setprovideroutputsource modesetting NVIDIA-0
    xrandr --auto
  '' else '''') + ''
    # Disable access control for the current user.
    xhost +SI:localuser:$USER

    # Set themes, etc.
    xrdb ~/.Xresources

    # Stop creating lots of serverauth files
    xserverauthfile=$XAUTHORITY

    # Make Java applications aware this is a non-reparenting window manager.
    export _JAVA_AWT_WM_NONREPARENTING=1

    # always keep numlock on
    # numlockx &

    # Finally start Emacs
    # exec emacsclient -c
    exec emacs
  '';
  home.file.".Xresources".text = ''
    Xcursor.size: 32
    Xcursor.theme: breeze_cursors
  '';
}
