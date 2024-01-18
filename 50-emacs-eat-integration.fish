# Emacs Eat terminal integration

# Significant portions of code based on the existing upstream bash and
# zsh integration.  The original copyright might apply, see:
# https://codeberg.org/akib/emacs-eat

# Features missing compared to the bash/zsh integrations:
# - Fish history import into Emacs.
# - The prompt markers are not portable, they depend on this specific config.
# - The _eat_msg function.
# - The PS2 prompt support (line continuation).

function eat_enable_integration
    function __eat_chpwd --on-variable PWD
        # Send the current working directory, for directory tracking.
        printf '\e]51;e;A;%s;%s\e\\\\'         \
            "$(echo -n -- $hostname | base64)" \
            "$(echo -n -- $PWD | base64)"
    end

    function __eat_preexec --on-event fish_preexec
        set current_command $argv[1]
        # Send current command.
        printf '\e]51;e;F;%s\e\\\\' \
            "$(echo -n -- $current_command | base64)"

        # Send pre-exec sequence.
        printf '\e]51;e;G\e\\\\'

        # Update title to include the command running.
        # "${PWD/$HOME/'~'}" converts "/home/akib/foo/" to "~/foo/".
        # The next one is substituted with '$', or '#' if we're "root".
        printf '\e]2;%s@%s:%s%s\e\\\\' "$USER" "$hostname" \
            "$(string replace $HOME '~' $PWD)"             \
            "$(fish_is_root_user && echo '#' || echo '$')" \
            "$current_command"
    end

    function __eat_postexec --on-event fish_postexec
        set exit_status $status
        # Send exit status.
        printf '\e]51;e;H;%i\e\\\\' $exit_status

        # Inform that a new prompt is going to be printed.
        printf '\e]51;e;J\e\\\\'

        # Update title.
        # "${PWD/$HOME/'~'}" converts "/home/akib/org/" to "~/org/".
        # The next one is substituted with '$', or '#' if we're "root".
        printf '\e]2;%s@%s:%s%s\e\\\\' "$USER" "$hostname" \
            "$(string replace $HOME '~' $PWD)"             \
            "$(fish_is_root_user && echo '#' || echo '$')"
    end

    # TODO: These are my own custom variables, not native to Fish.
    set --global --prepend fish_before_prompt \
        (printf '\e]51;e;B\e\\\\')
    set --global --append  fish_after_prompt  \
        (printf '\e]51;e;C\e\\\\')
end


if status is-interactive
    and set -q EAT_SHELL_INTEGRATION_DIR
    and string match -q "eat-*" $TERM

    eat_enable_integration
end
