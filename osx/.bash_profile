# Location: ~

# add locations to path
export PATH=~/bin:/usr/local/bin:/usr/local/sbin:$PATH

# add android sdk tools to path
export PATH=$PATH:/Applications/android-sdk-macosx/tools:/Applications/android-sdk-macosx/platform-tools

# put https://github.com/leftnode/get-shit-done on path
export PATH=$PATH:~/source/get-shit-done

# put stuff for Bher Church on path and create vicare env variable
export VICARE_LIBRARY_PATH=~/source/bher:~/source/scheme-tools
export PATH=$PATH:$VICARE_LIBRARY_PATH

# add installed python packages to python path
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"

# add npm to path
export PATH=$PATH:/usr/local/share/npm/bin

# set java home
export JAVA_HOME=`/usr/libexec/java_home -v 1.7`

# add colors to terminal
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# add alias definitions
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
