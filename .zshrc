# Global setting
bindkey -e
EDITOR=vim

# Complete
autoload -U compinit; compinit
setopt auto_list
setopt auto_menu
setopt list_packed
setopt list_types
bindkey "^[[Z" reverse-menu-complete
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt extended_glob
setopt equals
setopt correct
zstyle ':completion:*:default' menu select=1

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
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# Color
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Title
case "${TERM}" in
kterm*|xterm)
    precmd () {
        print -Pn "\e]0;%m:%~\a"
    };;
esac

# Prompt
autoload -U colors; colors
[ -f /usr/lib/git-core/git-sh-prompt ] && source /usr/lib/git-core/git-sh-prompt
setopt prompt_subst
git_prompt='$(__git_ps1 " (\e[01;32m%s\e[00m)")'
PROMPT="%n: %{${fg[red]}%}%~%{${reset_color}%}${git_prompt}
$ "
RPROMPT=''
SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "

# Alias
alias ls="ls --color"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias be='bundle exec'
alias less='less -R'
alias emacs='XMODIFIERS=@im=none emacs'
alias grep="grep --color=auto"
alias -g P='| peco'

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# environment
export CRYSTAL_CACHE_DIR=$HOME/.crystal

# Functions
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
                    eval $tac | \
                    peco --query "$LBUFFER")
    CURSOR=$#BUFFER
}
zle -N peco-select-history
bindkey '^r' peco-select-history
