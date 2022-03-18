# Global setting
bindkey -e
EDITOR=vim
zstyle ':completion:*' use-cache true

# Complete
_cache_hosts=($(egrep '^Host\s+[a-z0-9._-]+$' ~/.ssh/config | cut -d' ' -f2))
fpath=(~/.zsh $fpath)
autoload -U compinit; compinit
setopt auto_list
setopt auto_menu
setopt list_packed
setopt list_types
bindkey "^[[Z" reverse-menu-complete
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt equals
setopt correct
setopt completealiases
setopt magic_equal_subst
zstyle ':completion:*:default' menu select=1
[ -f /usr/share/zsh/vendor-completions/_docker ] && source /usr/share/zsh/vendor-completions/_docker

# History
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt share_history
setopt hist_reduce_blanks
setopt no_flow_control
setopt extended_history
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward

# Keybind
bindkey "^U" backward-kill-line

# Color
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Title
case "${TERM}" in
kterm*|xterm*|rxvt*)
    precmd () {
        print -Pn "\e]0;%m:%~\a"
    };;
esac

# Prompt
autoload -U colors; colors
[ -f /usr/lib/git-core/git-sh-prompt ] && source /usr/lib/git-core/git-sh-prompt
setopt prompt_subst
git_prompt='$(__git_ps1 " (\e[01;32m%s\e[00m)")'
PROMPT="%n: %B%{${fg[red]}%}%~%f%b%r${git_prompt}
$ "
RPROMPT='%{$fg[green]%} %D{%Y-%m-%d} %* %{$reset_color%}'
SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%f%b "

# Alias
alias ls="ls --color"
alias rm="rm -i"
alias cp="cp -iv"
alias mv="mv -i"
alias be='bundle exec'
alias emacs='XMODIFIERS=@im=none emacs'
alias e='emacsclient -n'
alias sudo='sudo -E '
alias brspec='bundle exec rspec -c'
alias grep='grep --color=auto'
alias today='date +%Y%m%d'
alias -g ag='ag --nogroup'
alias -g P='| peco'
alias -g L='| less'
alias -g G='| grep'
alias -g B='$(git branch | grep -Ev "^\*" | peco --layout bottom-up --prompt "GIT BRANCH> ")'
alias -g NP='--no-pager'
alias -g S='| sort'
alias o=xdg-open
alias ssh='TERM=xterm-256color ssh'
alias cdiff='git diff --no-index'
alias logcolor='sed -e "s/INFO/\x1b[32mINFO\x1b[0m/g" -e "s/WARN/\x1b[33mWARN\x1b[0m/g" -e "s/ERROR/\x1b[31mERRO\x1b[0m/g"'
alias g='git'
compdef g=git
alias y='yarn'
alias b='bundle'
compdef b=bundle
alias dc='docker compose'
compdef dc=docker-compose

if [ -x /usr/bin/lsd ]; then
  alias ls=lsd
  compdef ls=lsd
fi

# Plugin
if [ -d ~/.zsh/zsh-autosuggestions ]; then
  source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# rbenv
if [ -d $HOME/.rbenv ]; then
  export PATH=$HOME/.rbenv/bin:$PATH
  eval "$(rbenv init - zsh)"
fi

# nodenv
if [ -d $HOME/.nodenv ]; then
  export PATH=$HOME/.nodenv/bin:$PATH
  eval "$(nodenv init -)"
fi

# bookmarks
bookmark_dir=$HOME/.config/bookmarks
if [ -d $bookmark_dir ]; then
  export CDPATH=".:$bookmark_dir"
  alias goto="cd -P"
  _goto() {
    _files -W $bookmark_dir
  }
  compdef _goto goto

  function mkbm() {
    local dest=$bookmark_dir/@$(basename $1)
    if [ -e $dest ]; then
      echo 'already exist' $dest
    else
      ln -s $(realpath $1) $dest
    fi
  }
fi

# environment
export CRYSTAL_CACHE_DIR=$HOME/.crystal
export WORDCHARS="*?_-.[]~=&;!#$%^(){}<>"
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/bin:$PATH"
export LESS="-RFXiM"

# Functions
if which peco > /dev/null; then
  function peco-select-history() {
    local tac
    if which tac > /dev/null; then
      tac="tac"
    else
      tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
               eval $tac | \
               awk '!a[$0]++' | \
               peco --layout bottom-up --query "$LBUFFER")
    CURSOR=$#BUFFER
  }
  zle -N peco-select-history
  bindkey '^r' peco-select-history
fi

function mkcd() {
  mkdir -p "$@" && cd "$_"
}

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Other
xset -r 49 >/dev/null 2>&1 || :
