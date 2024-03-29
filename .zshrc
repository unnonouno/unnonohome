bindkey -e
autoload -U compinit; compinit -u
autoload -U colors; colors
autoload -U zargs

# env
EDITOR=vi
PAGER='lv -c'

# history
HISTFILE="$HOME/.zhistory"
HISTSIZE=10000
SAVEHIST=10000

# option, limit, bindkey
setopt hist_ignore_all_dups
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

if [ `uname` = Linux ]; then
    alias pbcopy="xsel --clipboard --input"
    alias open="xdg-open"
fi

#if [ $TERM = "cygwin" ]; then 
PROMPT='%{'$'\e[1;32m%}$USER@%m %{'$'\e[1;30m%} %~%{'$'\e[m%}\n%# '
#PS1=$'%{\e]2; %~ \a'$'$fg[green]%~%{'$'\e[m%} \n%# '

# foreground color
local N=$[0x`hostname | md5sum | cut -b-7`%6]
local R=$[0x`echo "$HOST"r | md5sum | cut -b-7`%192 + 64]
local G=$[0x`echo "$HOST"g | md5sum | cut -b-7`%192 + 64]
local B=$[0x`echo "$HOST"b | md5sum | cut -b-7`%192 + 64]
#local COL=$'%{\e[0;$[31+N]m%}'
local COL=$'%{\e[0;38;2;'$R';'$G';'$B'm%}'
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

# lv
if [ -f /usr/share/source-highlight/src-hilite-lesspipe.sh ]; then
  SRC_HIGHLIGHT=/usr/share/source-highlight/src-hilite-lesspipe.sh
fi
if [ -f /usr/local/bin/src-hilite-lesspipe.sh ]; then
  SRC_HIGHLIGHT=/usr/local/bin/src-hilite-lesspipe.sh
fi
if [ $SRC_HIGHLIGHT ]; then
  function lc() { if [ -f $1 ]; then $SRC_HIGHLIGHT $1 | lv -c; fi; }
fi

# jq
function jql() { jq -C $* | lv -c }

# grep
#  bold and red
export GREP_COLOR='01;31'
alias grep='grep --color=auto -s'
alias grepc='grep --color=always'

export PKG_CONFIG_PATH=$HOME/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
export PATH=$HOME/bin:$HOME/Library/Haskell/bin:/usr/texbin:/usr/local/bin:$PATH

# for c/c++
export INCLUDE_PATH=$HOME/include:/usr/local/include:$INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$INCLUDE_PATH
export C_INCLUDE_PATH=$INCLUDE_PATH
if [ -d /opt/local/lib ]; then
    export LIBRARY_PATH=/opt/local/lib:$LIBRARY_PATH
    export LD_LIBRARY_PATH=/opt/local/lib:$LD_LIBRARY_PATH
fi
export LIBRARY_PATH=$HOME/lib:/usr/local/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$HOME/lib:/usr/local/lib:$LD_LIBRARY_PATH

export PATH=$HOME/bin:/usr/local/bin:/opt/local/bin:$PATH

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
    if [ -d $HOME/.pyenv/plugins/pyenv-virtualenv ]; then
        eval "$(pyenv virtualenv-init -)"
    fi
fi

# cudnnenv
if [ -d $HOME/.cudnn ]; then
  export CFLAGS="-I$HOME/.cudnn/active/cuda/include $CFLAGS"
  export LDFLAGS="-L$HOME/.cudnn/active/cuda/lib64 $LDFLAGS"
  export LD_LIBRARY_PATH=$HOME/.cudnn/active/cuda/lib64:$LD_LIBRARY_PATH
fi

# GO
# export GOROOT=$HOME/go
# export GOPATH=$HOME/dev/go
# export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# Cargo
if [ -d $HOME/.cargo/bin ]; then
    export PATH=$HOME/.cargo/bin:$PATH
fi

# CUDA
if [ -d /usr/local/cuda ]; then
    export CUDA_HOME=/usr/local/cuda
fi
if [ $CUDA_HOME ]; then
  export PATH=${CUDA_HOME}/bin:$PATH
  export LD_LIBRARY_PATH=${CUDA_HOME}/lib64:$LD_LIBRARY_PATH
fi

# torch
TORCH_HOME=$HOME/torch/install
if [ -d $TORCH_HOME ]; then
  export PATH=$TORCH_HOME/bin:$PATH
  export LD_LIBRARY_PATH=$TORCH_HOME/lib:$LD_LIBRARY_PATH
  export DYLD_LIBRARY_PATH=$TORCH_HOME/lib:$DYLD_LIBRARY_PATH
fi

# mkl
MKL_HOME=/opt/intel/mkl
if [ -d $MKL_HOME ]; then
  export LD_LIBRARY_PATH=$MKL_HOME/lib/intel64:$LD_LIBRARY_PATH
fi

# ccache
if [ -d /usr/lib/ccache ]; then
  export PATH=/usr/lib/ccache:$PATH
fi

# direnv
if type direnv > /dev/null 2>&1; then
    eval "$(direnv hook zsh)"
fi

# ssh-agent
agent="$HOME/.ssh/agent"
if [ -S "$SSH_AUTH_SOCK" ]; then
    case $SSH_AUTH_SOCK in
    /tmp/*/agent.[0-9]*)
        ln -snf "$SSH_AUTH_SOCK" $agent && export SSH_AUTH_SOCK=$agent
    esac
elif [ -S $agent ]; then
    export SSH_AUTH_SOCK=$agent
else
    echo "no ssh-agent"
fi

# xkb
if [ -s $HOME/.xkb/keymap/mykbd ]; then
  xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mykbd $DISPLAY 2> /dev/null
fi

# anyenv
if [ -s $HOME/.anyenv ]; then
  export PATH="$HOME/.anyenv/bin:$PATH"
  eval "$(anyenv init -)"
fi
