# Location: ~

# add locations to path
export PATH=/usr/local/bin:/usr/local/sbin:$PATH

# add android sdk tools to path
export PATH=$PATH:/Applications/android-sdk-macosx/tools:/Applications/android-sdk-macosx/platform-tools

# put https://github.com/leftnode/get-shit-done on path
export PATH=$PATH:~/source/get-shit-done

# configure emacs commands for emacs 24
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias emacsclient='/usr/local/Cellar/emacs/HEAD/bin/emacsclient'

# set emacs as default editor
export EDITOR=emacs

# add colors to terminal
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# add alias definitions
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
