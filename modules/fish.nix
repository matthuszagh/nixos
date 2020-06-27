{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    vendor.config.enable = true;
    vendor.completions.enable = true;
    vendor.functions.enable = true;
  };

  # users.users.matt = {
  #   shell = "/run/current-system/sw/bin/fish";
  # };

  home-manager.users.matt = { ... }: {
    programs.fish = {
      enable = true;
      shellAliases = {
        ll = "${pkgs.coreutils}/bin/ls -Alh";
        rm = "${pkgs.trash-cli}/bin/trash";
      };
      interactiveShellInit = ''
        function vterm_printf;
            if [ -n "$TMUX" ]
                # tell tmux to pass the escape sequences through
                # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
            else if string match -q -- "screen*" "$TERM"
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]%s\007\e\\" "$argv"
            else
                printf "\e]%s\e\\" "$argv"
            end
        end

        function fish_vterm_prompt_end;
            vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
        end
        function track_directories --on-event fish_prompt; fish_vterm_prompt_end; end

        if test "$INSIDE_EMACS" = "vterm"
            function clear;
                vterm_printf "51;Evterm-clear-scrollback";
                tput clear;
            end

            function ff;
                vterm_cmd find-file "(realpath "$argv")"
            end
        end

        function vterm_cmd
            if test -n "$TMUX"
                # tell tmux to pass the escape sequences through
                # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                vterm_printf "\ePtmux;\e\e]51;E"
            else if test "$TERM" = "screen"
                # GNU screen (screen, screen-256color, screen-256color-bce)
                vterm_printf "\eP\e]51;E"
            else
                vterm_printf "\e]51;E"
            end

            vterm_printf "\e]51;E"
            set r ""
            for x in $argv
                set r $x (string replace '\\' '\\\\')
                set r $r (string replace '/\"' '\\\"')
                vterm_printf '"%s" ' "$r"
            end
            if test -n "$TMUX"
                # tell tmux to pass the escape sequences through
                # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                vterm_printf "\007\e\\"
            else if test "$TERM" = "screen"
                # GNU screen (screen, screen-256color, screen-256color-bce)
                vterm_printf "\007\e\\"
            else
                vterm_printf "\e\\"
            end
        end

        eval (direnv hook fish)
      '';
    };

    # home.file.".config/fish/fish_prompt.fish".text = ''
    #   set -l nix_shell_info (
    #     if test "$IN_NIX_SHELL" = "1"
    #       echo -n "<nix-shell> "
    #     end
    #   )
    # '';
  };
}
