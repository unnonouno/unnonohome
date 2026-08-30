# Basic editing and completion
bindkey -e
autoload -U colors; colors
autoload -U compinit; compinit -i
autoload -U history-search-end
autoload -U zargs

# Environment
export EDITOR=vi
if command -v lv >/dev/null 2>&1; then
  export PAGER='lv -c'
else
  export PAGER='less -R'
fi

# Keep PATH entries unique, and add only paths that exist.
typeset -U path PATH
path_prepend() {
  local dir
  local -a dirs
  for dir in "$@"; do
    [[ -d "$dir" ]] && dirs+=("$dir")
  done
  path=($dirs $path)
}
path_append() {
  local dir
  local -a dirs
  for dir in "$@"; do
    [[ -d "$dir" ]] && dirs+=("$dir")
  done
  path=($path $dirs)
}
pathvar_prepend() {
  local var=$1
  shift
  local dir entry
  local -a dirs current unique
  for dir in "$@"; do
    [[ -d "$dir" ]] && dirs+=("$dir")
  done
  current=("${(@ps.:.)${(P)var}}")
  for entry in "${dirs[@]}" "${current[@]}"; do
    [[ -n "$entry" && ${unique[(Ie)$entry]} -eq 0 ]] && unique+=("$entry")
  done
  export "$var=${(j.:.)unique}"
}
path_prepend "$HOME/bin" "/usr/local/bin" "/opt/local/bin" "$HOME/Library/Haskell/bin" "/usr/texbin"

# History
HISTFILE="$HOME/.zhistory"
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_all_dups
setopt share_history

# Shell behavior and key bindings
setopt prompt_subst
setopt ignore_eof
setopt complete_aliases
unsetopt promptcr

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end  history-search-end
bindkey '^N' history-beginning-search-forward-end
bindkey '^P' history-beginning-search-backward-end

# OS-specific aliases
case "$(uname)" in
  Darwin)
    alias ls='ls -G'
    alias la='ls -aG'
    alias ll='ls -alhG'
    ;;
  Linux)
    alias ls='ls --color'
    alias la='ls -a --color'
    alias ll='ls -alh --color'
    alias pbcopy='xsel --clipboard --input'
    alias open='xdg-open'
    ;;
esac

# Prompt
_hash_prefix() {
  if command -v md5sum >/dev/null 2>&1; then
    print -r -- "$1" | md5sum | cut -b 1-7
  elif command -v md5 >/dev/null 2>&1; then
    print -r -- "$1" | md5 -q | cut -b 1-7
  else
    print -r -- '0000000'
  fi
}

local R=$(( 0x$(_hash_prefix "${HOST}r") % 192 + 64 ))
local G=$(( 0x$(_hash_prefix "${HOST}g") % 192 + 64 ))
local B=$(( 0x$(_hash_prefix "${HOST}b") % 192 + 64 ))
local COL=$'%{\e[0;38;2;'$R';'$G';'$B'm%}'
local DEFAULT=$'%{\e[1;m%}'
PS1=$COL$'$HOST:%~'$DEFAULT$'\n%# '

# Show a short title in screen/tmux-compatible terminals that report TERM=screen.
if [[ "$TERM" = screen ]]; then
  PROMPT=$'\033k%(4~,%-1~/.../%2~,%~)\033\134'$PROMPT

  preexec() {
    local -a cmd
    cmd=(${(z)1})
    echo -n $'\033k'$cmd[1]$'\033\134'
  }
fi

# Global aliases
alias -g G='| grep '
alias -g L='| less '
alias -g H='| head '
alias -g T='| tail '
alias -g V='| vi '
alias -g N='| nkf -s'
alias -g C=' | /usr/local/bin/clip '
alias -g P=' < /dev/clipboard '

alias clean='rm *~'

# Pager helpers
page() {
  ${=PAGER}
}

# source-highlight through the selected pager.
SRC_HIGHLIGHT=''
if [[ -f /usr/share/source-highlight/src-hilite-lesspipe.sh ]]; then
  SRC_HIGHLIGHT=/usr/share/source-highlight/src-hilite-lesspipe.sh
elif [[ -f /usr/local/bin/src-hilite-lesspipe.sh ]]; then
  SRC_HIGHLIGHT=/usr/local/bin/src-hilite-lesspipe.sh
fi
if [[ -n "$SRC_HIGHLIGHT" ]]; then
  lc() {
    [[ -f "$1" ]] && "$SRC_HIGHLIGHT" "$1" | page
  }
fi

jql() {
  jq -C "$@" | page
}

# grep and diff
export GREP_COLORS='mt=01;31'
alias grep='grep --color=auto -s'
alias grepc='grep --color=always'

dif() {
  diff -y -W "$(tput cols)" --color=always "$@" | page
}

# Build paths
export PKG_CONFIG_PATH="$HOME/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/pkgconfig"
export INCLUDE_PATH="$HOME/include:/usr/local/include:${INCLUDE_PATH:-}"
export CPLUS_INCLUDE_PATH="$INCLUDE_PATH"
export C_INCLUDE_PATH="$INCLUDE_PATH"
pathvar_prepend LIBRARY_PATH "$HOME/lib" "/usr/local/lib"
pathvar_prepend LD_LIBRARY_PATH "$HOME/lib" "/usr/local/lib"
pathvar_prepend LIBRARY_PATH "/opt/local/lib"
pathvar_prepend LD_LIBRARY_PATH "/opt/local/lib"

export GTEST_COLOR=yes

# OCaml / godi / opam
if [[ -d "$HOME/godi" ]]; then
  path_prepend "$HOME/godi/bin" "$HOME/godi/sbin"
  export MANPATH="$HOME/godi/man:${MANPATH:-}"
fi

if [[ -d "$HOME/.opam" ]]; then
  source "$HOME/.opam/opam-init/init.zsh" >/dev/null 2>&1 || true
fi

# Ruby
if [[ -d "$HOME/.rbenv" ]]; then
  export RBENV_ROOT="$HOME/.rbenv"
  path_prepend "$RBENV_ROOT/bin"
  command -v rbenv >/dev/null 2>&1 && eval "$(rbenv init - zsh)"
fi

# Python
if [[ -d "$HOME/.pyenv" ]]; then
  export PYENV_ROOT="$HOME/.pyenv"
  path_prepend "$PYENV_ROOT/bin"
  if command -v pyenv >/dev/null 2>&1; then
    eval "$(pyenv init -)"
    if [[ -d "$PYENV_ROOT/plugins/pyenv-virtualenv" ]]; then
      eval "$(pyenv virtualenv-init -)"
    fi
  fi
fi

# CUDA / cuDNN
if [[ -d "$HOME/.cudnn" ]]; then
  export CFLAGS="-I$HOME/.cudnn/active/cuda/include ${CFLAGS:-}"
  export LDFLAGS="-L$HOME/.cudnn/active/cuda/lib64 ${LDFLAGS:-}"
  pathvar_prepend LD_LIBRARY_PATH "$HOME/.cudnn/active/cuda/lib64"
fi

if [[ -d /usr/local/cuda ]]; then
  export CUDA_HOME=/usr/local/cuda
fi
if [[ -n "${CUDA_HOME:-}" ]]; then
  path_prepend "$CUDA_HOME/bin"
  pathvar_prepend LD_LIBRARY_PATH "$CUDA_HOME/lib64"
fi

# Torch / MKL
TORCH_HOME="$HOME/torch/install"
if [[ -d "$TORCH_HOME" ]]; then
  path_prepend "$TORCH_HOME/bin"
  pathvar_prepend LD_LIBRARY_PATH "$TORCH_HOME/lib"
  pathvar_prepend DYLD_LIBRARY_PATH "$TORCH_HOME/lib"
fi

MKL_HOME=/opt/intel/mkl
if [[ -d "$MKL_HOME" ]]; then
  pathvar_prepend LD_LIBRARY_PATH "$MKL_HOME/lib/intel64"
fi

# Common toolchains
path_prepend "$HOME/.cargo/bin" "/usr/lib/ccache" "$HOME/.local/bin" "$HOME/Android/Sdk/platform-tools"

if [[ -d "$HOME/.anyenv" ]]; then
  path_prepend "$HOME/.anyenv/bin"
  command -v anyenv >/dev/null 2>&1 && eval "$(anyenv init -)"
fi

if command -v npm >/dev/null 2>&1; then
  path_append "$(npm prefix --location=global)/bin"
fi

export PNPM_HOME="$HOME/.local/share/pnpm"
path_prepend "$PNPM_HOME"

# direnv
if command -v direnv >/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# ssh-agent
agent="$HOME/.ssh/agent"
if [[ -S "${SSH_AUTH_SOCK:-}" ]]; then
  case "$SSH_AUTH_SOCK" in
    /tmp/*/agent.<->)
      ln -snf "$SSH_AUTH_SOCK" "$agent" && export SSH_AUTH_SOCK="$agent"
      ;;
  esac
elif [[ -S "$agent" ]]; then
  export SSH_AUTH_SOCK="$agent"
fi

# X keyboard map
if [[ -n "${DISPLAY:-}" && -s "$HOME/.xkb/keymap/mykbd" ]]; then
  xkbcomp -I"$HOME/.xkb" "$HOME/.xkb/keymap/mykbd" "$DISPLAY" 2>/dev/null
fi

# Google Cloud SDK
if [[ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]]; then
  source "$HOME/google-cloud-sdk/path.zsh.inc"
fi
if [[ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]]; then
  source "$HOME/google-cloud-sdk/completion.zsh.inc"
fi

# Command completions
if command -v op >/dev/null 2>&1; then
  eval "$(op completion zsh)"
  compdef _op op
fi

if command -v uv >/dev/null 2>&1; then
  eval "$(uv generate-shell-completion zsh)"
fi

if [[ -f "$HOME/.pf-completion.zsh" ]]; then
  source "$HOME/.pf-completion.zsh"
fi
