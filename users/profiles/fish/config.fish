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
