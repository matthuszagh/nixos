{ config, pkgs, ... }:

{
  programs.bash = {
    shellAliases = {
      ls = "${pkgs.coreutils}/bin/ls --color=auto";
      ll = "${pkgs.coreutils}/bin/ls -Alh";
      rm = "${pkgs.trash-cli}/bin/trash";
    };
    enableCompletion = true;
    # promptInit = ''
    #   # Provide a nice prompt if the terminal supports it.
    #   if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
    #     PROMPT_COLOR="01;34m"
    #     PS1="\[\033[$PROMPT_COLOR\]\w\[\033[$PROMPT_COLOR\] \$ \[\033[00m\]"
    #   fi
    # '';
  };

  home-manager.users.matt = { ... }: {
    programs.bash = {
      enable = true;
      initExtra = ''
        function vterm_printf(){
            if [ -n "$TMUX" ]; then
                # tell tmux to pass the escape sequences through
                # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                printf "\ePtmux;\e\e]%s\007\e\\" "$1"
            elif [ "''${TERM%%-*}" = "screen" ]; then
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]%s\007\e\\" "$1"
            else
                printf "\e]%s\e\\" "$1"
            fi
        }

        if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
            function clear(){
                vterm_printf "51;Evterm-clear-scrollback";
                tput clear;
            }
        fi

        vterm_cmd() {
            if [ -n "$TMUX" ]; then
                # tell tmux to pass the escape sequences through
                # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                printf "\ePtmux;\e\e]51;E"
            elif [ "''${TERM%%-*}" = "screen" ]; then
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]51;E"
            else
                printf "\e]51;E"
            fi

            printf "\e]51;E"
            local r
            while [[ $# -gt 0 ]]; do
                r="''${1//\\/\\\\}"
                r="''${r//\"/\\\"}"
                printf '"%s" ' "$r"
                shift
            done
            if [ -n "$TMUX" ]; then
                # tell tmux to pass the escape sequences through
                # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                printf "\007\e\\"
            elif [ "''${TERM%%-*}" = "screen" ]; then
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\007\e\\"
            else
                printf "\e\\"
            fi
        }

        ff() {
            vterm_cmd find-file "$(realpath "$@")"
        }

        vterm_prompt_end(){
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
        }
        PS1=$PS1'\[$(vterm_prompt_end)\]'

        eval "$(direnv hook bash)"
      '';
    };
  };
}
