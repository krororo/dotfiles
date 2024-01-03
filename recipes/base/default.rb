dotfile '.peco'
dotfile '.vimrc'

xdg_config 'gem/gemrc'
xdg_config 'lsd/config.yaml'
xdg_config 'ov/config.yaml'

package 'lsd'

include_recipe '../../cookbooks/emacs'
include_recipe '../../cookbooks/gh'
include_recipe '../../cookbooks/git'
include_recipe '../../cookbooks/zsh'
