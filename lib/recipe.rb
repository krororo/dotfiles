include_recipe 'recipe_helper'

node.reverse_merge!(user: ENV['SUDO_USER'] || ENV['USER'])

[
  "#{ENV['HOME']}/bin",
  "#{ENV['HOME']}/.config/bookmarks",
  "#{ENV['HOME']}/.config/systemd/user",
].each do |dir|
  directory dir do
    owner node[:user]
  end
end

dotfile '.peco'
dotfile '.vimrc'

xdg_config 'gem/gemrc'
xdg_config 'gtk-3.0/gtk.css'
xdg_config 'lsd/config.yaml'

include_recipe '../cookbooks/docker'
include_recipe '../cookbooks/emacs'
include_recipe '../cookbooks/gh'
include_recipe '../cookbooks/git'
include_recipe '../cookbooks/lsd'
include_recipe '../cookbooks/rbenv'
include_recipe '../cookbooks/xremap'
include_recipe '../cookbooks/zsh'

package 'fcitx-mozc'
package 'fonts-noto-color-emoji'
