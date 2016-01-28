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
    set -l owner (ls -ld /usr/local | awk '{print $3, $4 }')
    if test $owner -ne "$USER admin"
        sudo chown -R $USER:admin /usr/local
        echo Changing permissions of /usr/local to $USER:admin
    end

        brew update
    and brew upgrade $argv
end
