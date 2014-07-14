bindkey -e
autoload -U compinit; compinit -u
autoload -U colors; colors

# env
EDITOR=vi
PAGER='lv -c'

# history
HISTFILE="$HOME/.zhistory"
HISTSIZE=10000
SAVEHIST=10000

# option, limit, bindkey
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt share_history

autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end  history-search-end
bindkey '^N' history-beginning-search-forward-end
bindkey '^P' history-beginning-search-backward-end

setopt prompt_subst
setopt ignore_eof

#PSCOLOR='00;04;32'      # 細字;下線;緑
#RPROMPT=$'%{\e[${PSCOLOR}m%}[%~]%{\e[00m%}' # 右プロンプト
#PS1="$ "

if [ `uname` = Darwin ]; then
  alias ls="ls -G"
  alias la="ls -aG"
  alias ll="ls -alhG"
else
  alias ls="ls --color"
  alias la="ls -a --color"
  alias ll="ls -alh --color"
fi

#if [ $TERM = "cygwin" ]; then 
PROMPT='%{'$'\e[1;32m%}$USER@%m %{'$'\e[1;30m%} %~%{'$'\e[m%}\n%# '
#PS1=$'%{\e]2; %~ \a'$'$fg[green]%~%{'$'\e[m%} \n%# '

# foreground color
local N=$[0x`hostname | md5sum | cut -b-7`%6]
local COL=$'%{\e[0;$[31+N]m%}'
local GREEN=$'%{\e[0;32m%}'
local BLUE=$'%{\e[0;34m%}'
local MAGENTA=$'%{\e[0;35m%}'
local CYAN=$'%{\e[0;36m%}'
local DEFAULT=$'%{\e[1;m%}'
PS1=$COL$'$HOST:%~'$DEFAULT$'\n%# '
#RPROMPT='[%D{%H:%M on %a}]'

#fi

unsetopt promptcr

#PROMPT="%U$USER@%m %#%u "
#RPROMPT="%~"

if [ "$TERM" = "screen" ]; then
  PROMPT=$'\033k%(4~,%-1~/.../%2~,%~)\033\134'$PROMPT

  preexec() {
    local -a cmd; cmd=(${(z)1})
    echo -n $'\033k'$cmd[1]$'\033\134'
  }
fi

alias -g G='| grep '
alias -g L='| less '
alias -g H='| head '
alias -g T='| tail '
alias -g V='| vi '
alias -g N='| nkf -s'

alias -g C=' | /usr/local/bin/clip '
alias -g P=' < /dev/clipboard '

alias clean='rm *~'


# grep
#  bold and red
export GREP_COLOR='01;31'
export GREP_OPTIONS='--color=auto'
alias grepc='grep --color=always'

export PKG_CONFIG_PATH=$HOME/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
export PATH=$HOME/bin:$HOME/Library/Haskell/bin:/usr/texbin:/usr/local/bin:$PATH

# for c/c++
export INCLUDE_PATH=$HOME/include:/usr/local/include:$INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$INCLUDE_PATH
export C_INCLUDE_PATH=$INCLUDE_PATH
export LIBRARY_PATH=$HOME/lib:/usr/local/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$HOME/lib:/usr/local/lib:$LD_LIBRARY_PATH
export PATH=$HOME/bin:/usr/local/bin:$PATH

# for gtest
export GTEST_COLOR=yes

# for godi
if [ -d $HOME/godi ]; then
    export PATH=$HOME/godi/bin:$HOME/godi/sbin:$PATH
    export MANPATH=$HOME/godi/man:$MANPATH
fi

# OPAM configuration
if [ -d $HOME/.opam ]; then
  . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

# rbenv
if [ -d $HOME/.rbenv ]; then
    export RBENV_ROOT=$HOME/.rbenv
    export PATH=$RBENV_ROOT/bin:${PATH}
    eval "$(rbenv init - zsh)"
fi

# pyenv
if [ -d $HOME/.pyenv ]; then
    export PYENV_ROOT=$HOME/.pyenv
    export PATH=$PYENV_ROOT/bin:${PATH}
    eval "$(pyenv init -)"
fi

# GO
export GOROOT=$HOME/go
export GOPATH=$HOME/dev/go
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# cask
if [ -d $HOME/.cask]; then
    export PATH=$HOME/.cask/bin:${PATH}
fi
