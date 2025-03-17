include_recipe "../../cookbooks/brew"

dotfile '.vimrc'

xdg_config 'gem/gemrc'
xdg_config 'irb/irbrc'
xdg_config 'lsd/config.yaml'
xdg_config 'ov/config.yaml'
xdg_config "solargraph/config.yml"

package 'lsd'
package 'jq'

include_recipe "../../cookbooks/bat"
include_recipe '../../cookbooks/emacs'
include_recipe '../../cookbooks/fd'
include_recipe '../../cookbooks/gh'
include_recipe '../../cookbooks/git'
include_recipe '../../cookbooks/mise'
include_recipe "../../cookbooks/wezterm"
include_recipe '../../cookbooks/zsh'
