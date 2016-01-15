# ~/Dropbox/conf/fish/mac.config.fish
#
# Setup:
#
# Place the following line in ~/.config/fish/config.fish
# source ~/dotfiles/configs/config/fish/mac.config.fish

# Prompt
set __fish_prompt_hostname "mac-fnord"
set fish_greeting "Guten Tag, Herr "(whoami)"!"

# Mac specific functions
function macupg
    sudo softwareupdate -i -a $argv
end
function brewupg
    # Homebrew permission denied on upgrades, getting old
    sudo chown -R $USER /usr/local/bin
    sudo chown -R $USER /usr/local/share
    sudo chown -R $USER /usr/local/Library
    sudo chown -R $USER /usr/local/Cellar

        brew update
    and brew upgrade $argv
end
