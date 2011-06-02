bindkey -e

autoload -U compinit; compinit -u

autoload -U colors; colors

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
  alias ll="ls -alG"
else
  alias ls="ls --color"
  alias la="ls -a --color"
  alias ll="ls -al --color"
fi

#if [ $TERM = "cygwin" ]; then 
PROMPT='%{'$'\e[1;32m%}$USER@%m %{'$'\e[1;30m%} %~%{'$'\e[m%}\n%# '
#PS1=$'%{\e]2; %~ \a'$'$fg[green]%~%{'$'\e[m%} \n%# '

# foreground color
local N=$[0x`hostname | md5sum | cut -b-8`%5]
local COL=$'%{\e[0;$[32+N]m%}'
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

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/opt/local/lib/pkgconfig

#eval `dircolors -b ~/.dir_colors`


# for c/c++
export INCLUDE_PATH=/opt/local/include:$INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$INCLUDE_PATH
export LIBRARY_PATH=/opt/local/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH
export PATH=/opt/local/bin:$PATH
