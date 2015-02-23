# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/haetze/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall


export PATH=/usr/local/bin:$PATH
export PATH=~/Documents/Code/go/bin:$PATH

export GOPATH=~/Documents/Code/go

alias appleScript=osascript

alias ttytter="ttytter -ssl -dostream"


