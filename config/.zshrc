# Global setting
bindkey -e
EDITOR=vim
zstyle ':completion:*' use-cache true

# Complete
if type brew &>/dev/null; then
  brew_prefix=$(brew --prefix)
  FPATH=${brew_prefix}/share/zsh-completions:$FPATH
  fpath=(${brew_prefix}/share/zsh/site-functions $fpath)
fi
if [ -f ~/.ssh/config ]; then
  _cache_hosts=($(grep -E '^Host\s+[a-z0-9._-]+$' ~/.ssh/config | cut -d' ' -f2))
fi
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
if [ -f /usr/lib/git-core/git-sh-prompt ]; then
  source /usr/lib/git-core/git-sh-prompt
elif [ -f /opt/homebrew/etc/bash_completion.d/git-prompt.sh ]; then
  source /opt/homebrew/etc/bash_completion.d/git-prompt.sh
fi
setopt prompt_subst
git_prompt='$(__git_ps1 " (\e[01;32m%s\e[00m)")'
PROMPT="%n: %B%{${fg[red]}%}%~%f%b%r${git_prompt}
$ "
RPROMPT=''
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
alias ssh='TERM=xterm-256color ssh'
alias cdiff='git diff --no-index'
alias logcolor='sed -e "s/INFO/\x1b[32mINFO\x1b[0m/g" -e "s/WARN/\x1b[33mWARN\x1b[0m/g" -e "s/ERROR/\x1b[31mERRO\x1b[0m/g"'
alias g='git'
compdef g=git
alias b='bundle'
compdef b=bundle
alias dc='docker compose'
compdef dc=docker-compose
alias p='pnpm'

if type xdg-open &>/dev/null; then
  alias o=xdg-open
else
  alias o=open
fi

if type lsd &>/dev/null; then
  alias ls=lsd
  compdef ls=lsd
fi

if [ -d /opt/homebrew ] && ls /opt/homebrew/opt/*/libexec/gnubin &>/dev/null; then
  for d in /opt/homebrew/opt/*/libexec/gnubin; do
    PATH=$d:$PATH
  done
fi

alias gae-ssh='(){ gcloud --project $1 app instances ssh $(gcloud --project $1 --format json app instances list --service $2 --sort-by="~instance.startTime" | jq -r ".[0].id") --service $2 --version $(gcloud --project $1 --format json app instances list --service $2 --sort-by="~instance.startTime" | jq -r ".[0].version") }'

# Plugin
if [ -d ~/.zsh/zsh-autosuggestions ]; then
  source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

if which mise > /dev/null; then
  eval "$(mise activate zsh)"
  export PATH=$HOME/.local/share/mise/shims:$PATH
  export MISE_NODE_DEFAULT_PACKAGES_FILE=$HOME/.config/mise/node-default-pkgs
  export MISE_NODE_COREPACK=1
  export MISE_RUBY_DEFAULT_PACKAGES_FILE=$HOME/.config/mise/ruby-default-gems
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
export WORDCHARS="*?_-.[]~=&;!#$%^(){}<>"
export PATH="$HOME/bin:$PATH"
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

# The next line updates PATH for the Google Cloud SDK.
if [ -f $HOME'/google-cloud-sdk/path.zsh.inc' ]; then . $HOME'/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f $HOME'/google-cloud-sdk/completion.zsh.inc' ]; then . $HOME'/google-cloud-sdk/completion.zsh.inc'; fi

# tabtab source for packages
# uninstall by removing these lines
if [ -f ~/.config/tabtab/zsh/__tabtab.zsh ]; then
  . ~/.config/tabtab/zsh/__tabtab.zsh || true
  compdef p=pnpm
fi

if [ -f /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
  source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
elif [ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
