include_recipe 'recipe_helper'

node.reverse_merge!(user: ENV['SUDO_USER'] || ENV['USER'])

directory "#{ENV['HOME']}/bin" do
  owner node[:user]
end

directory "#{ENV['HOME']}/.config/systemd/user" do
  owner node[:user]
end

dotfile '.docker'
dotfile '.peco'
dotfile '.vimrc'

xdg_config 'gem/gemrc'
xdg_config 'gtk-3.0/gtk.css'
xdg_config 'lsd/config.yaml'

include_recipe '../cookbooks/emacs'
include_recipe '../cookbooks/gh'
include_recipe '../cookbooks/git'
include_recipe '../cookbooks/rbenv'
include_recipe '../cookbooks/xremap'
include_recipe '../cookbooks/zsh'

package 'fonts-noto-color-emoji'
