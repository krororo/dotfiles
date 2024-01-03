dotfile '.peco'
dotfile '.vimrc'

xdg_config 'gem/gemrc'
xdg_config 'lsd/config.yaml'
xdg_config 'ov/config.yaml'

package 'lsd'
package 'zsh-syntax-highlighting'

include_recipe '../../cookbooks/emacs'
include_recipe '../../cookbooks/git'
