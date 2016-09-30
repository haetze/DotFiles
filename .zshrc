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
export PATH=~/Documents/Code/lfe/bin:$PATH
#export PATH=~/Documents/Code/haskell/MyCalendar/bin:$PATH

export GOPATH=~/Documents/Code/go

alias appleScript=osascript
alias xbat="acpiconf -i 0"
alias xmobarbat="xbat | grep \"Remaining capacity\" "

alias ttytter="ttytter -ssl -dostream"

export TZ=America/Los_Angeles


autoload -Uz vcs_info
precmd () { vcs_info }
setopt prompt_subst
PS1="%~\$vcs_info_msg_0_>"

# OPAM configuration
. /home/haetze/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

fpath=(~/.zsh/ $fpath)
autoload -U compinit
compinit
